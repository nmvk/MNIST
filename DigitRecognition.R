#MNIST Digit recognition using SVM & PCA
#Author Raghav Nayak M

library(e1071)

#Read training and test dataset
train <- read.csv("train.csv", header=TRUE)
test <- read.csv("test.csv", header=TRUE)
labels <- train[,1]
train <- train[,-1]


#Start applying PCA for dimentionality reduction
nw <- cov(train);

nd <- svd(nw);

u <- nd$u[,1:120];

train_names <- colnames(train);
train_names <- train_names[1:120];

train <- as.matrix(train);

train <- train %*% u

colnames(train) <- train_names;

dataset =data.frame(label=as.factor(labels), train);

#Train SVM with polynomial kernel of degree 9
model = svm(label ~., data = dataset, kernel = "polynomial", degree = 2)

test <- as.matrix(test);

test <- test %*% u;

#Column names for training dataset and testing would be same :)
colnames(test) <- train_names;

#Prepare output file
pred = predict(model, newdata = test)
predictions = data.frame(ImageId=1:nrow(test), Label=pred)
write.csv(predictions,"predictions-new.csv", row.names= FALSE)