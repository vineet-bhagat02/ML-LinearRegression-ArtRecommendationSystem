
######################################################################################
#This R script is building a linear model on Image, Text and Image-Text Feature set  #
#This script should be executed after Image_Text_Features_withMajorityVotes_Preprocessing.py  execution         #
#Experiment 1 -  LM Model for Image Feature Set                                      #
#Experiment 2 -  LM Model for Text Feature Set                                       #
#Experiment 3 -  LM Model for Image & Text Feature combined Set                      #
######################################################################################

########## Experiment 1 with Image Files ##############################################

#Set source directory location where all the source code and data files are placed
setwd("C:/Users/vinee/PycharmProjects/pythonProject")
options(scipen = 999)
#image_feat_lab<-read.csv('all_feat_lab_main_transpose.csv',header=TRUE, sep=",",)
image.feat.lab<-read.csv('all_feat_lab_main.csv',header=TRUE, sep=",",)



image.feat.lab<-image.feat.lab[-1]

str(image.feat.lab)
image.feat.lab<-image.feat.lab[complete.cases(image.feat.lab), ]

#Building a feature vector for tree, animal and mythical labels
img.tree.labels <- subset(image.feat.lab, select=-c(Lable_Mythical,Lable_Animal,file))
img.mythical.labels<- subset(image.feat.lab, select=-c(Lable_Tree,Lable_Animal,file))
img.animal.labels <- subset(image.feat.lab, select=-c(Lable_Tree,Lable_Mythical,file))


#Splitting the data into train and test dataset into 80:20 split
#Total Images (main jpg files)= 31, so #train dataset=24
img.tree.train.rows<-sample(nrow(img.tree.labels), 24)
img.tree.train.set<-img.tree.labels[img.tree.train.rows,]
img.tree.test.set<-img.tree.labels[-img.tree.train.rows,]

img.mythical.train.rows<-sample(nrow(img.mythical.labels), 24)
img.mythical.train.set<-img.mythical.labels[img.mythical.train.rows,]
img.mythical.test.set<-img.mythical.labels[-img.mythical.train.rows,]

img.animal.train.rows<-sample(nrow(img.animal.labels), 24)
img.animal.train.set<-img.animal.labels[img.animal.train.rows,]
img.animal.test.set<-img.animal.labels[-img.animal.train.rows,]

#Removing NAs 
str(img.animal.train.set)
img.animal.train.set<-img.animal.train.set[complete.cases(img.animal.train.set), ]


# Fitting a Linear Model on the train dataset
img.tree_model <- lm(Lable_Tree ~ ., data = img.tree.train.set)
img.mythical_model <- lm(Lable_Mythical ~ ., data = img.mythical.train.set)
img.animal_model <- lm(Lable_Animal ~ ., data = img.animal.train.set)

#Summary Stats of the LM Model
summary(img.tree_model)
summary(img.mythical_model)
summary(img.animal_model)


#Prediction on Test Data
img.Pred_tree <- predict(img.tree_model, img.tree.test.set)
img.Pred_mythical <- predict(img.mythical_model, img.mythical.test.set)
img.Pred_animal <- predict(img.animal_model, img.animal.test.set)

#Mean Squared Error on Test set
img_tree_mse = mean((img.tree.test.set$Lable_Tree - img.Pred_tree) ^ 2)
img_mythical_mse = mean((img.mythical.test.set$Lable_Mythical - img.Pred_mythical) ^ 2)
img_animal_mse = mean((img.animal.test.set$Lable_Animal - img.Pred_animal) ^ 2)

img_tree_mse
img_mythical_mse
img_animal_mse




########## Experiment 2 with Text Files ###############################################

#Reading Text Feature file created using Python script
text_feat_lab<-read.csv('text_feat_lab.csv',header=TRUE, sep=",",)

#Building a feature vector for tree, animal and mythical labels
tree.labels <- subset(text_feat_lab, select=-c(Label_Mythical,Label_Animal,file,ImageFileName))
mythical.labels<- subset(text_feat_lab, select=-c(Label_Tree,Label_Animal,file,ImageFileName))
animal.labels <- subset(text_feat_lab, select=-c(Label_Tree,Label_Mythical,file,ImageFileName))

#Splitting the tree, mythical and animal data into train and test dataset with 80:20 split
tree.train.rows<-sample(nrow(tree.labels), 20)
tree.train.set<-tree.labels[tree.train.rows,]
tree.test.set<-tree.labels[-tree.train.rows,]

mythical.train.rows<-sample(nrow(mythical.labels), 20)
mythical.train.set<-mythical.labels[mythical.train.rows,]
mythical.test.set<-mythical.labels[-mythical.train.rows,]

animal.train.rows<-sample(nrow(animal.labels), 20)
animal.train.set<-animal.labels[animal.train.rows,]
animal.test.set<-animal.labels[-animal.train.rows,]

# Fitting a Linear Model on the train dataset
tree_model <- lm(Label_Tree ~ ., data = tree.train.set)
mythical_model <- lm(Label_Mythical ~ ., data = mythical.train.set)
animal_model <- lm(Label_Animal ~ ., data = animal.train.set)

#Summary Stats of the LM Model
summary(tree_model)
summary(mythical_model)
summary(animal_model)

#Prediction on Test Data
Pred_tree <- predict(tree_model, tree.test.set)
Pred_mythical <- predict(mythical_model, mythical.test.set)
Pred_animal <- predict(animal_model, animal.test.set)

#Evaluating performance of the model using Root Mean Squared Error
error<-tree.test.set$Label_Tree-Pred_tree
RMSE <- sqrt(mean(error^2))
RMSE

#Mean Squared Error on Test data set
text_tree_mse = mean((tree.test.set$Label_Tree - Pred_tree) ^ 2)
text_mythical_mse = mean((mythical.test.set$Label_Mythical - Pred_mythical) ^ 2)
text_animal_mse = mean((animal.test.set$Label_Animal - Pred_animal) ^ 2)

text_tree_mse
text_mythical_mse
text_animal_mse





########## Experiment 3 with Image & Text Files ###############################################

#Merging Image and Text features generated above
img.text.feat.lab<-merge(x = image.feat.lab, y = text_feat_lab, by.x = "file",by.y = "ImageFileName", all.x = TRUE)

#Removing NAs
img.text.feat.lab<-img.text.feat.lab[complete.cases(img.text.feat.lab), ]

#Building a feature vector for tree, animal and mythical labels
img.txt.tree.labels <- subset(img.text.feat.lab, select=-c(Lable_Mythical,Lable_Animal,file))
img.txt.mythical.labels<- subset(img.text.feat.lab, select=-c(Lable_Tree,Lable_Animal,file))
img.txt.animal.labels <- subset(img.text.feat.lab, select=-c(Lable_Tree,Lable_Mythical,file))

# Fitting a Linear Model on the train dataset
img.txt.tree.model <- lm(Lable_Tree ~ ., data = img.txt.tree.labels)
img.txt.mythical.model <- lm(Lable_Mythical ~ ., data = img.txt.mythical.labels)
img.txt.animal.model <- lm(Lable_Animal ~ ., data = img.txt.animal.labels)

#Summary Stats of the LM Model
summary(img.txt.tree.model)
summary(img.txt.mythical.model)
summary(img.txt.animal.model)



































text_feat_lab<-read.csv('text_feat_lab.csv',header=TRUE, sep=",",)

image.feat.lab
colnames(text_feat_lab)

img.text.feat.lab<-merge(x = image.feat.lab, y = text_feat_lab, by.x = "file",by.y = "ImageFileName", all.x = TRUE)
colnames(img.text.feat.lab)

img.text.feat.lab.cleaned<-img.text.feat.lab[complete.cases(img.text.feat.lab), ]


tree.labels <- subset(img.text.feat.lab.cleaned, select=-c(Lable_Mythical,Lable_Animal,file))
mythical.labels<- subset(img.text.feat.lab.cleaned, select=-c(Lable_Tree,Lable_Animal,file))
animal.labels <- subset(img.text.feat.lab.cleaned, select=-c(Lable_Tree,Lable_Mythical,file))

nrow(tree.labels)

tree.train.rows<-sample(nrow(tree.labels), 20)
tree.train.set<-tree.labels[tree.train.rows,]
tree.test.set<-tree.labels[-tree.train.rows,]

mythical.train.rows<-sample(nrow(mythical.labels), 20)
mythical.train.set<-mythical.labels[mythical.train.rows,]
mythical.test.set<-mythical.labels[-mythical.train.rows,]

animal.train.rows<-sample(nrow(animal.labels), 20)
animal.train.set<-animal.labels[animal.train.rows,]
animal.test.set<-animal.labels[-animal.train.rows,]

tree_model <- lm(Lable_Tree ~ ., data = tree.train.set)
mythical_model <- lm(Lable_Mythical ~ ., data = mythical.train.set)
animal_model <- lm(Lable_Animal ~ ., data = animal.train.set)


summary(tree_model)

