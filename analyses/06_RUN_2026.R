
library(dplyr)
library(ggplot2)

# Logger horodaté (pour suivi console)
logit <- function(...) cat(sprintf("[%s] %s\n", format(Sys.time(), "%H:%M:%S"), sprintf(...)))


###############################
### PREPARATION DES DONNEES ###
###############################

load("outputs/05_bio_plongees_pressions_env_clean.RData")

cv_seed = 121

# Suppression des habitats trop peu représentés
hab_remove <- c("III.2.2. SFBC",
                "IV.2.3.b. Faciès à L. phalangium",
                "IV.2.4. SGCF - circalittoral")

df_base <- df_base[!(df_base$cd_hab %in% hab_remove),]

# Palette de couleurs
pal_hab <- c(
  "III.3.2. SGCF - infralittoral" = "#ffffbf",
  "III.3.2.b. SGCF - Pralines" = "#d307b4",
  "IV.2.2. DC" = "#e5dbae",
  "IV.2.2.a. Maërl DC"   = "#ff0000",
  "IV.2.2.b. Pralines DC"  = "#d307b4",
  "IV.2.2.b. Boxworks DC"  = "#0007b4",
  "IV.2.2.c. Peyssonnelia libres"   = "#33a02c"
)

colScale <- scale_fill_manual(name = "Categories :", values = alpha(pal_hab, 0.7))

# Transformation des habitats en facteur
df_base$cd_hab <- as.factor(df_base$cd_hab)
df_base$surfstat <- as.factor(df_base$surfstat)

# Rescaling
df_raw_scaled <- df_base
num_cols <- names(df_base)[sapply(df_base, is.numeric)]
if (length(num_cols)) df_raw_scaled[num_cols] <- scale(df_base[num_cols])
df_raw_scaled$cd_hab <- droplevels(df_raw_scaled$cd_hab)


############
### FAMD ###
############

logit("FAMD (exploratoire)…")
famd_png1 <- file.path("figures", "06_FAMD_contrib_dim1.png")
famd_png2 <- file.path("figures", "06_FAMD_contrib_dim2.png")
famd_pngi <- file.path("figures", "06_FAMD_individus.png")
famd_csv  <- file.path("outputs", "06_FAMD_contributions.csv")

res_famd <- FactoMineR::FAMD(df_base, sup.var = which(names(df_base) == "cd_hab"), graph = FALSE)
p_dim1 <- factoextra::fviz_contrib(res_famd, choice = "var", axes = 1, top = 15) + ggtitle("Top variables – Dim.1")
p_dim2 <- factoextra::fviz_contrib(res_famd, choice = "var", axes = 2, top = 15) + ggtitle("Top variables – Dim.2")
ggsave(famd_png1, p_dim1, width = 7, height = 5, dpi = 300)
ggsave(famd_png2, p_dim2, width = 7, height = 5, dpi = 300)
contrib_df <- as.data.frame(res_famd$var$contrib); contrib_df$Variable <- rownames(contrib_df)
readr::write_csv(contrib_df, famd_csv)
ind_coord <- as.data.frame(res_famd$ind$coord); ind_coord$cd_hab <- df_base$cd_hab
eig <- factoextra::get_eigenvalue(res_famd)
p_ind <- ggplot(ind_coord, aes(Dim.1, Dim.2, color = cd_hab)) +
  geom_point(size=1.8, alpha=.85) +
  stat_ellipse(aes(group=cd_hab), linetype="dashed") +
  scale_color_manual(values = pal_hab) +
  labs(x = sprintf("Dim.1 (%.1f%%)", eig[1,"variance.percent"]),
       y = sprintf("Dim.2 (%.1f%%)", eig[2,"variance.percent"]),
       title = "FAMD — projection des individus") +
  theme(legend.position = "bottom")
ggsave(famd_pngi, p_ind, width=8, height=6, dpi=300)



######################################################
### CHOIX DES HYPERPARAMETRES PAR CROSS-VALIDATION ###
######################################################

# Suppression de surfstat s'il existe car ne doit pas être un prédicteur !
#df_base$surfstat <- NULL
#df_raw_scaled$surfstat <- NULL

predictors <- setdiff(names(df_raw_scaled), c("cd_hab", "w"))

# ---------- 3bis) Tuning RF (10-fold CV) ----------
#Recherche de mtry & min.node.size sur grille avec reprise automatique (CSV "partial")
logit("Tuning RF (10-fold CV) — checkpoints…")
set.seed(cv_seed)
df_for_cv <- if ("w" %in% names(df_raw_scaled)) {
  df_raw_scaled[, c("cd_hab", predictors, "w"), drop = FALSE]
} else {
  df_raw_scaled[, c("cd_hab", predictors), drop = FALSE]
}
cv10 <- rsample::vfold_cv(df_for_cv, v = 10, strata = cd_hab)     # stratification par classe
p <- length(predictors)
grid <- unique(expand.grid(
  mtry = pmax(2, round(c(sqrt(p), p/4, p/3))),
  min.node.size = c(1,5,10)
))

# Fichiers de checkpoints (reprise)
path_final   <- file.path("outputs", "06_RF_tuning_cv10.csv")
path_best    <- file.path("outputs", "06_RF_best_params.csv")
path_partial <- file.path("outputs", "06_RF_tuning_cv10_partial.csv")

# Tuning
# Évalue l’accuracy moyenne par pli (prédiction de la classe majoritaire)
partial <- data.frame()
for (i in seq_len(nrow(grid))) {
  mtry_i <- grid$mtry[i]; mns_i <- grid$min.node.size[i]
  acc <- purrr::map_dbl(cv10$splits, function(s){
    tr <- rsample::analysis(s); te <- rsample::assessment(s)
    cw_tr <- if ("w" %in% names(tr)) tr$w else NULL
    fit <- ranger::ranger(cd_hab ~ .,
                          data = tr |> dplyr::select(all_of(c("cd_hab", predictors))),
                          num.trees = 300,
                          mtry = mtry_i, min.node.size = mns_i,
                          importance = "none",
                          case.weights = cw_tr,
                          classification = TRUE, probability = FALSE,
                          respect.unordered.factors = "order",
                          save.memory = TRUE,
                          num.threads = max(1, parallel::detectCores() - 1))
    pred_class <- predict(fit, data = te[, predictors, drop = FALSE], type = "response")$predictions
    mean(pred_class == te$cd_hab)
  })
  newrow <- tibble(mtry = mtry_i, min.node.size = mns_i,
                   acc_mean = mean(acc), acc_sd = sd(acc))
  partial <- bind_rows(partial, newrow)
  readr::write_csv(partial, path_partial)  # 🧷 checkpoint après chaque combo
  logit("Tuning progress: mtry=%s, min.node.size=%s -> acc=%.3f", mtry_i, mns_i, newrow$acc_mean)
}

# Figer l’ordre final + extraire le meilleur set d’hyperparamètres
cv_res <- partial |> arrange(desc(acc_mean))
readr::write_csv(cv_res, path_final)
best <- cv_res[1,]
readr::write_csv(best, path_best)
logit("Best RF: mtry=%d, min.node.size=%d, acc=%.3f ± %.3f",
      best$mtry, best$min.node.size, best$acc_mean, best$acc_sd)


################################
### IMPORTANCE DES VARIABLES ###
################################

# Entraînements finaux : 2 variantes d’importance (Permutation / Gini)
cw_all <- if ("w" %in% names(df_raw_scaled)) df_raw_scaled$w else NULL

#Chemins RDS des modèles finaux (permet reprise)
rds_perm <- file.path("outputs", "06_RF_final_perm.rds")
rds_gini <- file.path("outputs", "06_RF_final_gini.rds")

# PERM
set.seed(123)
rf_perm <- ranger::ranger(cd_hab ~ .,
                          data = df_raw_scaled |> dplyr::select(all_of(c("cd_hab", predictors))),
                          num.trees = 1000, mtry = best$mtry, min.node.size = best$min.node.size,
                          importance = "permutation", case.weights = cw_all,
                          classification = TRUE, probability = TRUE,
                          respect.unordered.factors = "order",
                          num.threads = max(1, parallel::detectCores() - 1))
saveRDS(rf_perm, rds_perm)

# GINI
set.seed(123)
rf_gini <- ranger::ranger(cd_hab ~ .,
                          data = df_raw_scaled |> dplyr::select(all_of(c("cd_hab", predictors))),
                          num.trees = 1000, mtry = best$mtry, min.node.size = best$min.node.size,
                          importance = "impurity", case.weights = cw_all,
                          classification = TRUE, probability = TRUE,
                          respect.unordered.factors = "order",
                          num.threads = max(1, parallel::detectCores() - 1))
saveRDS(rf_gini, rds_gini)


# Importances & VIP (avec checkpoints CSV/PNG)
imp_perm_csv <- file.path("outputs", "06_RF_importance_permutation.csv")
imp_gini_csv <- file.path("outputs", "06_RF_importance_gini.csv")
vip_perm_png <- file.path("figures", "06_RF_vip_perm.png")
vip_gini_png <- file.path("figures", "06_RF_vip_gini.png")

imp_perm <- sort(rf_perm$variable.importance, decreasing = TRUE)
imp_perm_df <- tibble(Variable = names(imp_perm), MeanDecreaseAccuracy = as.numeric(imp_perm))
readr::write_csv(imp_perm_df, imp_perm_csv)

imp_gini <- sort(rf_gini$variable.importance, decreasing = TRUE)
imp_gini_df <- tibble(Variable = names(imp_gini), MeanDecreaseGini = as.numeric(imp_gini))
readr::write_csv(imp_gini_df, imp_gini_csv)

p1 <- vip::vip(rf_perm, num_features = 20) + ggtitle("Importance (Permutation / MDA)")
ggsave(vip_perm_png, p1, width = 7, height = 5, dpi = 300)

p2 <- vip::vip(rf_gini, num_features = 20) + ggtitle("Importance (Gini)")
ggsave(vip_gini_png, p2, width = 7, height = 5, dpi = 300)


# Top-15 variables (Permutation/MDA) — exporté pour réutilisation
top15 <- setdiff(imp_perm_df$Variable[1:min(15, nrow(imp_perm_df))], c("w","n","n_class"))
readr::write_csv(tibble(TopRF = top15), file.path("outputs", "06_Top15_RF.csv"))


#######################################################################
### EVALUATION DES PERFORMANCES DU MODELE AVEC MEILLEURS PARAMETRES ###
#######################################################################

# Évalue via CV 10-fold (mêmes folds que tuning) : AUC OVA, CM, Accuracy, Kappa, F1 macro
logit("Évaluation 10-fold (AUC/F1/CM)…")
metrics_csv <- file.path("outputs", "06_Metrics_macro_CV.csv")

set.seed(cv_seed)
pred_cv <- purrr::map_dfr(cv10$splits, function(s){
  tr <- rsample::analysis(s); te <- rsample::assessment(s)
  cw_tr <- if ("w" %in% names(tr)) tr$w else NULL
  fit <- ranger::ranger(cd_hab ~ .,
                        data = tr |> dplyr::select(all_of(c("cd_hab", predictors))),
                        num.trees = 400,
                        mtry = best$mtry, min.node.size = best$min.node.size,
                        case.weights = cw_tr,
                        classification = TRUE, probability = TRUE,
                        respect.unordered.factors = "order",
                        save.memory = TRUE,
                        num.threads = max(1, parallel::detectCores() - 1))
  pr <- predict(fit, data = te[, predictors, drop = FALSE])$predictions
  cbind(te["cd_hab"], as.data.frame(pr))
})

# AUC one-vs-all par classe + AUC macro
classes <- levels(df_raw_scaled$cd_hab)
auc_ova <- sapply(classes, function(cls){
  y_bin <- factor(pred_cv$cd_hab == cls, levels = c(FALSE, TRUE))
  pROC::auc(pROC::roc(y_bin, pred_cv[[cls]], quiet = TRUE))
})
auc_macro <- mean(as.numeric(auc_ova))
readr::write_csv(data.frame(Class = classes, AUC = as.numeric(auc_ova)),
                 file.path("outputs", "06_AUC_one_vs_all_CV.csv"))
writeLines(sprintf("AUC_macro_CV,%.4f", auc_macro),
           file.path("outputs", "06_AUC_macro_CV.txt"))

# Matrice de confusion + métriques globales
prob_cols <- colnames(pred_cv)[-1]
pred_label <- factor(
  prob_cols[max.col(as.matrix(pred_cv[, prob_cols, drop = FALSE]))],
  levels = classes
)
eval_df <- pred_cv |> dplyr::transmute(.truth = factor(cd_hab, levels = classes),
                                       .pred_class = pred_label)
cm <- yardstick::conf_mat(eval_df, truth = .truth, estimate = .pred_class)
readr::write_csv(as.data.frame(cm$table), file.path("outputs", "06_ConfusionMatrix_CV.csv"))

acc <- yardstick::accuracy(eval_df, .truth, .pred_class)
kap <- yardstick::kap(eval_df, .truth, .pred_class)
f1m <- yardstick::f_meas(eval_df, .truth, .pred_class, estimator = "macro")
dplyr::bind_rows(acc, kap, f1m) |> readr::write_csv(metrics_csv)


### PLOT CM 
cm_table <- table(eval_df$.truth, eval_df$.pred_class)

cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Reel", "Pred", "Freq")

cm_df$Reel <- factor(cm_df$Reel)
cm_df$Pred <- factor(cm_df$Pred)

# Calculer les proportions PAR LIGNE (normalisation par classe réelle)
cm_df <- cm_df %>%
  group_by(Reel) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  ungroup()

# Inverser l'ordre des prédictions pour avoir une diagonale inversée
cm_df$Pred <- factor(cm_df$Pred, levels = rev(sort(unique(cm_df$Pred))))

# Calculer le nombre d'occurrences par classe réelle
class_counts <- cm_df %>%
  group_by(Reel) %>%
  summarise(Total = sum(Freq))

# Créer les labels Y avec le nombre total d'occurrences
y_labels <- paste0(class_counts$Reel, " (n=", class_counts$Total, ")")

# Plot ggplot
cm <- ggplot(cm_df, aes(x = Pred, y = Reel, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Proportion)),
            color = "black", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#e0f3f8", high = "#0077b6") +
  scale_y_discrete(labels = y_labels) +  # Afficher le total sous les labels
  labs(
    title = "Matrice de confusion",
    x = "Prédiction",
    y = "Réel"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" # Supprimer la légende
  )

ggsave(plot = cm, filename = "figures/06_CM.jpg", width = 2700, height = 700, units = "px")


############################################################
### SI REGROUPEMENT DE TOUTES LES CLASSES DE RHODOLITHES ###
############################################################

eval_df_rhodo <- eval_df
eval_df_rhodo$.truth <- as.character(eval_df_rhodo$.truth)
eval_df_rhodo$.truth[!(eval_df_rhodo$.truth %in% "IV.2.2. DC")] <- "Rhodolithes"
eval_df_rhodo$.pred_class <- as.character(eval_df_rhodo$.pred_class)
eval_df_rhodo$.pred_class[!(eval_df_rhodo$.pred_class %in% "IV.2.2. DC")] <- "Rhodolithes"
eval_df_rhodo$.truth <- as.factor(eval_df_rhodo$.truth)
eval_df_rhodo$.pred_class <- as.factor(eval_df_rhodo$.pred_class)

cm_rhodo <- yardstick::conf_mat(eval_df_rhodo, truth = .truth, estimate = .pred_class)
readr::write_csv(as.data.frame(cm$table), file.path("outputs", "06_ConfusionMatrix_CV.csv"))

acc_rhodo <- yardstick::accuracy(eval_df_rhodo, .truth, .pred_class)
kap_rhodo <- yardstick::kap(eval_df_rhodo, .truth, .pred_class)
f1m_rhodo <- yardstick::f_meas(eval_df_rhodo, .truth, .pred_class, estimator = "macro")
dplyr::bind_rows(acc_rhodo, kap_rhodo, f1m_rhodo) |> readr::write_csv(metrics_csv)


### PLOT CM 
cm_table <- table(eval_df_rhodo$.truth, eval_df_rhodo$.pred_class)

cm_df <- as.data.frame(cm_table)
colnames(cm_df) <- c("Reel", "Pred", "Freq")

cm_df$Reel <- factor(cm_df$Reel)
cm_df$Pred <- factor(cm_df$Pred)

# Calculer les proportions PAR LIGNE (normalisation par classe réelle)
cm_df <- cm_df %>%
  group_by(Reel) %>%
  mutate(Proportion = Freq / sum(Freq)) %>%
  ungroup()

# Inverser l'ordre des prédictions pour avoir une diagonale inversée
cm_df$Pred <- factor(cm_df$Pred, levels = rev(sort(unique(cm_df$Pred))))

# Calculer le nombre d'occurrences par classe réelle
class_counts <- cm_df %>%
  group_by(Reel) %>%
  summarise(Total = sum(Freq))

# Créer les labels Y avec le nombre total d'occurrences
y_labels <- paste0(class_counts$Reel, " (n=", class_counts$Total, ")")

# Plot ggplot
cm <- ggplot(cm_df, aes(x = Pred, y = Reel, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Proportion)),
            color = "black", size = 4, fontface = "bold") +
  scale_fill_gradient(low = "#e0f3f8", high = "#0077b6") +
  scale_y_discrete(labels = y_labels) +  # Afficher le total sous les labels
  labs(
    title = "Matrice de confusion",
    x = "Prédiction",
    y = "Réel"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "none" # Supprimer la légende
  )

ggsave(plot = cm, filename = "figures/06_CM_rhodolithes.jpg", width = 1500, height = 600, units = "px")


###################################################
### MODELE FINAL POUR PROJECTION CARTOGRAPHIQUE ###
###################################################

set.seed(123)
rf_final <- ranger::ranger(cd_hab ~ .,
                          data = df_base |> dplyr::select(all_of(c("cd_hab", predictors))),
                          num.trees = 1000, mtry = best$mtry, min.node.size = best$min.node.size,
                          importance = "none", case.weights = cw_all,
                          classification = TRUE, probability = TRUE,
                          respect.unordered.factors = "order",
                          num.threads = max(1, parallel::detectCores() - 1))

saveRDS(rf_final, "outputs/06_RF_final_projection.rds")

save.image(file = "backup_RUN_final.RData")

