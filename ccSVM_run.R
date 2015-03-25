ccSVM <- function(X,train,test,y,L,lambda,C){

rescaled <- Rescaling(X,L,lambda)
X.new <- rescaled[1]
K.new <- rescaled[2]
l <- rescaled[3]

model.new <- svm(X.new,y,C)
pred <- predict(model.new,X.new,decision.values=TRUE)
dec <- attr(pred,'decision.values')
auc.comp <- auc(dec,test)


#Matlab stuff
# model.new = svmtrain(y(train), [(1:length(train))' K.new(train,train)], ['-c ' num2str(C) ' -t 4']);'
# [Predict.label, accuracy, dec] = svmpredict(y(test), [(1:length(test))' K.new(test,train)], model.new);' 
# label = model_new.Label
# auc = ComputAuc(dec,y(test),label(1),label(2))

#Matlab stuff
# svind = model_new.SVs
# sv = X.new(:,svind)
# alpha = model_new.sv_coef
# w.new = sv*alpha
# w = w.new./l

#return (c(predict.label, dec, accuracy, sv, w))
return (auc.comp)
}
