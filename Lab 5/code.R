# ============================================================
# 1. INSTALL PACKAGES - RUN ONLY ONCE
# ============================================================

if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

BiocManager::install("EBImage")

install.packages("magick")
install.packages("keras3")


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(EBImage)
library(magick)
library(keras3)


# ============================================================
# 3. INSTALL TENSORFLOW - RUN ONLY ONCE
# ============================================================

install_keras(backend = "tensorflow")


# ============================================================
# 4. SET WORKING DIRECTORY
# ============================================================

setwd("C:/Users/Soham/Desktop/Projects/R-submissions/Lab 5")


# ============================================================
# 5. READ IMAGE FILE NAMES
# ============================================================

pics <- c(
  "plane1.jpeg",
  "plane2.jpeg",
  "plane3.jpeg",
  "car1.jpeg",
  "car2.jpeg",
  "car3.jpeg"
)


# ============================================================
# 6. READ IMAGES
# ============================================================

mypic <- list()

for (i in 1:6) {
  mypic[[i]] <- image_read(pics[i])
}


# ============================================================
# 7. EXPLORE IMAGE
# ============================================================

mypic[[1]]

summary(mypic[[1]])


# ============================================================
# 8. IMAGE HISTOGRAM
# ============================================================

img <- image_read("car1.jpeg")

pixels <- image_data(
  img,
  channels = "gray"
)

pixels <- as.numeric(pixels)

hist(
  pixels,
  main = "Car Image Histogram",
  xlab = "Pixel Intensity",
  breaks = 256
)


# ============================================================
# 9. RESIZE ALL IMAGES TO 64 x 64
# ============================================================

images <- lapply(pics, function(x) {
  image_resize(
    image_read(x),
    "64x64!"
  )
})


# ============================================================
# 10. RESHAPE FIRST IMAGE
# ============================================================

image_vector <- as.numeric(
  image_data(
    images[[1]],
    channels = "rgb"
  )
)

length(image_vector)


# ============================================================
# 11. CONVERT ALL IMAGES INTO NUMERIC VECTORS
# ============================================================

x <- lapply(images, function(img) {
  as.numeric(
    image_data(
      img,
      channels = "rgb"
    )
  )
})


# ============================================================
# 12. ROW BIND ALL IMAGES
# ============================================================

x <- do.call(rbind, x)

dim(x)


# ============================================================
# 13. NORMALIZE PIXEL VALUES
# ============================================================

x <- x / 255


# ============================================================
# 14. CREATE LABELS
# ============================================================

y_labels <- c(
  "Plane",
  "Plane",
  "Plane",
  "Car",
  "Car",
  "Car"
)


# ============================================================
# 15. CONVERT LABELS TO NUMERIC
# ============================================================

y_numeric <- ifelse(
  y_labels == "Car",
  0,
  1
)


# ============================================================
# 16. ONE-HOT ENCODING
# ============================================================

y <- to_categorical(
  y_numeric,
  num_classes = 2
)


# ============================================================
# 17. CREATE KERAS SEQUENTIAL MODEL
# ============================================================

model <- keras_model_sequential(
  input_shape = c(ncol(x))
) |>
  layer_dense(
    units = 128,
    activation = "relu"
  ) |>
  layer_dense(
    units = 64,
    activation = "relu"
  ) |>
  layer_dense(
    units = 2,
    activation = "softmax"
  )


# ============================================================
# 18. DISPLAY MODEL
# ============================================================

summary(model)


# ============================================================
# 19. COMPILE MODEL
# ============================================================

model |> compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)


# ============================================================
# 20. TRAIN MODEL
# ============================================================

history <- model |> fit(
  x,
  y,
  batch_size = 32,
  epochs = 30
)


# ============================================================
# 21. EVALUATE MODEL
# ============================================================

evaluation <- model |> evaluate(
  x,
  y
)

print(evaluation)


# ============================================================
# 22. MAKE PREDICTIONS
# ============================================================

predictions <- model |> predict(x)


# ============================================================
# 23. GET PREDICTED CLASS
# ============================================================

predicted_class <- apply(
  predictions,
  1,
  which.max
)


# ============================================================
# 24. CONVERT 1/2 INTO CAR/PLANE
# ============================================================

predicted_labels <- ifelse(
  predicted_class == 1,
  "Car",
  "Plane"
)


# ============================================================
# 25. CREATE CLEAR PREDICTION OUTPUT
# ============================================================

prediction_result <- data.frame(
  Image = pics,
  Actual = y_labels,
  Predicted = predicted_labels,
  Car_Probability = round(predictions[, 1], 4),
  Plane_Probability = round(predictions[, 2], 4)
)


# ============================================================
# 26. DISPLAY PREDICTIONS
# ============================================================

print(prediction_result)