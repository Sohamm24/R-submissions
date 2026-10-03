retail <- read_excel("Online Retail.xlsx")

head(retail)
str(retail)
dim(retail)
summary(retail)

sum(grepl("^C", retail$InvoiceNo))

sum(retail$Quantity <= 0, na.rm = TRUE)

sum(retail$UnitPrice <= 0, na.rm = TRUE)

colSums(is.na(retail))

length(unique(na.omit(retail$CustomerID)))

retail_clean <- retail %>%
  filter(
    !grepl("^C", InvoiceNo),
    !is.na(CustomerID),
    Quantity > 0,
    UnitPrice > 0
  )

dim(retail_clean)


sum(grepl("^C", retail_clean$InvoiceNo))

sum(is.na(retail_clean$CustomerID))

sum(retail_clean$Quantity <= 0)

sum(retail_clean$UnitPrice <= 0)

colSums(is.na(retail_clean))

retail_clean <- retail_clean %>%
  mutate(
    TotalAmount = Quantity * UnitPrice
  )

head(retail_clean)
summary(retail_clean$TotalAmount)


reference_date <- max(retail_clean$InvoiceDate) + 1

customer_data <- retail_clean %>%
  group_by(CustomerID) %>%
  summarise(
    Recency = as.numeric(
      reference_date - max(InvoiceDate)
    ),
    Frequency = n_distinct(InvoiceNo),
    MonetaryValue = sum(TotalAmount),
    AverageTransactionValue = mean(TotalAmount),
    QuantityPurchased = sum(Quantity),
    PurchaseFrequency = n()
  ) %>%
  ungroup()

customer_data

dim(customer_data)

head(customer_data)

summary(customer_data)

colSums(is.na(customer_data))

nrow(customer_data)


cluster_features <- customer_data %>%
  dplyr::select(
    Recency,
    Frequency,
    MonetaryValue,
    AverageTransactionValue,
    QuantityPurchased,
    PurchaseFrequency
  )

Q1 <- apply(
  cluster_features,
  2,
  quantile,
  probs = 0.25
)

Q3 <- apply(
  cluster_features,
  2,
  quantile,
  probs = 0.75
)

IQR_values <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_values
upper_bound <- Q3 + 1.5 * IQR_values

lower_bound
upper_bound

outlier_counts <- sapply(
  1:ncol(cluster_features),
  function(i) {
    sum(
      cluster_features[, i] < lower_bound[i] |
        cluster_features[, i] > upper_bound[i]
    )
  }
)

names(outlier_counts) <- colnames(cluster_features)

outlier_counts

customer_clean <- customer_data %>%
  dplyr::filter(
    Recency >= lower_bound["Recency"],
    Recency <= upper_bound["Recency"],
    Frequency >= lower_bound["Frequency"],
    Frequency <= upper_bound["Frequency"],
    MonetaryValue >= lower_bound["MonetaryValue"],
    MonetaryValue <= upper_bound["MonetaryValue"],
    AverageTransactionValue >= lower_bound["AverageTransactionValue"],
    AverageTransactionValue <= upper_bound["AverageTransactionValue"],
    QuantityPurchased >= lower_bound["QuantityPurchased"],
    QuantityPurchased <= upper_bound["QuantityPurchased"],
    PurchaseFrequency >= lower_bound["PurchaseFrequency"],
    PurchaseFrequency <= upper_bound["PurchaseFrequency"]
  )

dim(customer_clean)

clustering_data <- customer_clean %>%
  dplyr::select(
    Recency,
    Frequency,
    MonetaryValue,
    AverageTransactionValue,
    QuantityPurchased,
    PurchaseFrequency
  )

scaled_data <- scale(clustering_data)

head(scaled_data)

summary(scaled_data)

set.seed(123)

wss <- numeric(10)

for (k in 1:10) {
  kmeans_model <- kmeans(
    scaled_data,
    centers = k,
    nstart = 25
  )
  
  wss[k] <- kmeans_model$tot.withinss
}

wss

if (!dir.exists("outputs")) {
  dir.create("outputs")
}

png(
  "outputs/elbow_method.png",
  width = 1200,
  height = 800
)

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  main = "Elbow Method for Optimal Number of Clusters",
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares"
)

dev.off()

plot(
  1:10,
  wss,
  type = "b",
  pch = 19,
  main = "Elbow Method for Optimal Number of Clusters",
  xlab = "Number of Clusters (K)",
  ylab = "Within-Cluster Sum of Squares"
)

set.seed(123)

kmeans_final <- kmeans(
  scaled_data,
  centers = 4,
  nstart = 25
)

kmeans_final

kmeans_final$size

customer_clean$Cluster <- factor(kmeans_final$cluster)

table(customer_clean$Cluster)

png(
  "outputs/kmeans_clusters.png",
  width = 1200,
  height = 800
)

fviz_cluster(
  kmeans_final,
  data = scaled_data,
  geom = "point",
  ellipse.type = "convex",
  main = "K-Means Customer Segmentation"
)

dev.off()

fviz_cluster(
  kmeans_final,
  data = scaled_data,
  geom = "point",
  ellipse.type = "convex",
  main = "K-Means Customer Segmentation"
)

silhouette_result <- silhouette(
  kmeans_final$cluster,
  dist(scaled_data)
)

silhouette_avg <- mean(
  silhouette_result[, "sil_width"]
)

silhouette_avg

png(
  "outputs/silhouette_plot.png",
  width = 1200,
  height = 800
)

plot(
  silhouette_result,
  main = "Silhouette Plot for K-Means Clustering"
)

dev.off()

plot(
  silhouette_result,
  main = "Silhouette Plot for K-Means Clustering"
)
aggregate(
  silhouette_result[, "sil_width"],
  by = list(Cluster = kmeans_final$cluster),
  FUN = mean
)

set.seed(123)

sample_size <- 500

sample_indices <- sample(
  1:nrow(scaled_data),
  sample_size
)

hierarchical_data <- scaled_data[sample_indices, ]

hierarchical_distance <- dist(
  hierarchical_data
)

hierarchical_model <- hclust(
  hierarchical_distance,
  method = "ward.D2"
)

hierarchical_model

png(
  "outputs/hierarchical_dendrogram.png",
  width = 1400,
  height = 900
)

plot(
  hierarchical_model,
  labels = FALSE,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Customers",
  ylab = "Height"
)

rect.hclust(
  hierarchical_model,
  k = 4,
  border = 2:5
)

dev.off()

plot(
  hierarchical_model,
  labels = FALSE,
  main = "Hierarchical Clustering Dendrogram",
  xlab = "Customers",
  ylab = "Height"
)

rect.hclust(
  hierarchical_model,
  k = 4,
  border = 2:5
)

pca_model <- prcomp(
  scaled_data,
  center = FALSE,
  scale. = FALSE
)

summary(pca_model)

pca_variance <- pca_model$sdev^2 /
  sum(pca_model$sdev^2)

pca_variance

cumulative_variance <- cumsum(pca_variance)

cumulative_variance

pca_data <- data.frame(
  PC1 = pca_model$x[, 1],
  PC2 = pca_model$x[, 2],
  Cluster = customer_clean$Cluster
)

head(pca_data)

png(
  "outputs/pca_customer_segments.png",
  width = 1200,
  height = 800
)

ggplot(
  pca_data,
  aes(
    x = PC1,
    y = PC2,
    color = Cluster
  )
) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Customer Segments using PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()

dev.off()

ggplot(
  pca_data,
  aes(
    x = PC1,
    y = PC2,
    color = Cluster
  )
) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Customer Segments using PCA",
    x = "Principal Component 1",
    y = "Principal Component 2",
    color = "Cluster"
  ) +
  theme_minimal()


cluster_profile <- customer_clean %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_MonetaryValue = mean(MonetaryValue),
    Avg_TransactionValue = mean(AverageTransactionValue),
    Avg_Quantity = mean(QuantityPurchased),
    Avg_PurchaseFrequency = mean(PurchaseFrequency)
  )

cluster_profile

cluster_profile <- cluster_profile %>%
  mutate(
    Customer_Percentage = Customers /
      sum(Customers) * 100
  )

cluster_profile

write.csv(
  cluster_profile,
  "outputs/cluster_profile.csv",
  row.names = FALSE
)

png(
  "outputs/cluster_monetary_value.png",
  width = 1200,
  height = 800
)

barplot(
  cluster_profile$Avg_MonetaryValue,
  names.arg = cluster_profile$Cluster,
  main = "Average Monetary Value by Customer Cluster",
  xlab = "Cluster",
  ylab = "Average Monetary Value"
)

dev.off()

png(
  "outputs/cluster_frequency.png",
  width = 1200,
  height = 800
)

barplot(
  cluster_profile$Avg_Frequency,
  names.arg = cluster_profile$Cluster,
  main = "Average Purchase Frequency by Customer Cluster",
  xlab = "Cluster",
  ylab = "Average Frequency"
)

dev.off()

customer_model_data <- customer_clean %>%
  mutate(
    HighValue = ifelse(Cluster == 3, 1, 0)
  )

table(customer_model_data$HighValue)

prop.table(
  table(customer_model_data$HighValue)
) * 100

model_features <- customer_model_data %>%
  dplyr::select(
    Recency,
    Frequency,
    MonetaryValue,
    AverageTransactionValue,
    QuantityPurchased,
    PurchaseFrequency
  )

model_target <- factor(
  customer_model_data$HighValue,
  levels = c(0, 1),
  labels = c("Regular", "HighValue")
)

model_target

set.seed(123)

train_indices <- sample(
  1:nrow(model_features),
  size = 0.8 * nrow(model_features)
)

X_train <- model_features[train_indices, ]
X_test <- model_features[-train_indices, ]

y_train <- model_target[train_indices]
y_test <- model_target[-train_indices]

dim(X_train)
dim(X_test)

table(y_train)
table(y_test)

svm_scaler <- scale(
  X_train
)

X_train_svm <- svm_scaler

X_test_svm <- scale(
  X_test,
  center = attr(svm_scaler, "scaled:center"),
  scale = attr(svm_scaler, "scaled:scale")
)

set.seed(123)

rf_model <- randomForest(
  x = X_train,
  y = y_train,
  ntree = 300,
  importance = TRUE
)

rf_model

rf_cm <- table(
  Actual = y_test,
  Predicted = rf_pred
)

rf_TP <- rf_cm["HighValue", "HighValue"]
rf_TN <- rf_cm["Regular", "Regular"]
rf_FP <- rf_cm["Regular", "HighValue"]
rf_FN <- rf_cm["HighValue", "Regular"]

rf_accuracy <- (rf_TP + rf_TN) / sum(rf_cm)

rf_precision <- rf_TP / (rf_TP + rf_FP)

rf_recall <- rf_TP / (rf_TP + rf_FN)

rf_f1 <- 2 * (
  rf_precision * rf_recall
) / (
  rf_precision + rf_recall
)

rf_roc <- roc(
  y_test,
  rf_prob,
  levels = c("Regular", "HighValue"),
  direction = "<"
)

rf_auc <- as.numeric(auc(rf_roc))

rf_accuracy
rf_precision
rf_recall
rf_f1
rf_auc

rf_cm

rf_importance <- importance(rf_model)

rf_importance

png(
  "outputs/random_forest_feature_importance.png",
  width = 1200,
  height = 800
)

varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

dev.off()

varImpPlot(
  rf_model,
  main = "Random Forest Feature Importance"
)

set.seed(123)

svm_model <- svm(
  x = X_train_svm,
  y = y_train,
  kernel = "radial",
  probability = TRUE
)

svm_model

svm_pred <- predict(
  svm_model,
  X_test_svm,
  probability = TRUE
)

svm_probabilities <- attr(
  svm_pred,
  "probabilities"
)

svm_prob <- svm_probabilities[, "HighValue"]

svm_cm <- table(
  Actual = y_test,
  Predicted = svm_pred
)

svm_TP <- svm_cm["HighValue", "HighValue"]
svm_TN <- svm_cm["Regular", "Regular"]
svm_FP <- svm_cm["Regular", "HighValue"]
svm_FN <- svm_cm["HighValue", "Regular"]

svm_accuracy <- (svm_TP + svm_TN) / sum(svm_cm)

svm_precision <- svm_TP / (svm_TP + svm_FP)

svm_recall <- svm_TP / (svm_TP + svm_FN)

svm_f1 <- 2 * (
  svm_precision * svm_recall
) / (
  svm_precision + svm_recall
)

svm_roc <- roc(
  y_test,
  svm_prob,
  levels = c("Regular", "HighValue"),
  direction = "<"
)

svm_auc <- as.numeric(
  auc(svm_roc)
)

svm_accuracy
svm_precision
svm_recall
svm_f1
svm_auc

svm_cm


model_comparison <- data.frame(
  Model = c("Random Forest", "SVM"),
  Accuracy = c(
    rf_accuracy,
    svm_accuracy
  ),
  Precision = c(
    rf_precision,
    svm_precision
  ),
  Recall = c(
    rf_recall,
    svm_recall
  ),
  F1_Score = c(
    rf_f1,
    svm_f1
  ),
  ROC_AUC = c(
    rf_auc,
    svm_auc
  )
)

model_comparison

write.csv(
  model_comparison,
  "outputs/model_comparison.csv",
  row.names = FALSE
)

png(
  "outputs/roc_curve_comparison.png",
  width = 1200,
  height = 800
)

plot(
  rf_roc,
  main = "ROC Curve Comparison",
  col = 1,
  lwd = 2
)

lines(
  svm_roc,
  col = 2,
  lwd = 2
)

legend(
  "bottomright",
  legend = c(
    paste("Random Forest AUC =", round(rf_auc, 4)),
    paste("SVM AUC =", round(svm_auc, 4))
  ),
  col = c(1, 2),
  lwd = 2
)

dev.off()

plot(
  rf_roc,
  main = "ROC Curve Comparison",
  col = 1,
  lwd = 2
)

lines(
  svm_roc,
  col = 2,
  lwd = 2
)

legend(
  "bottomright",
  legend = c(
    paste("Random Forest AUC =", round(rf_auc, 4)),
    paste("SVM AUC =", round(svm_auc, 4))
  ),
  col = c(1, 2),
  lwd = 2
)

plotly_3d <- plot_ly(
  customer_clean,
  x = ~Recency,
  y = ~Frequency,
  z = ~MonetaryValue,
  color = ~Cluster,
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 4)
)

plotly_3d <- plotly_3d %>%
  layout(
    title = "3D Customer Segmentation",
    scene = list(
      xaxis = list(title = "Recency"),
      yaxis = list(title = "Frequency"),
      zaxis = list(title = "Monetary Value")
    )
  )

plotly_3d

htmlwidgets::saveWidget(
  plotly_3d,
  "outputs/3d_customer_segmentation.html",
  selfcontained = TRUE
)

list.files("outputs")
