Data: Bank Dataset from UCI Repository https://archive.ics.uci.edu/ml/datasets/Bank+Marketing. 

Two separate classifiers (Decision Tree, and SVM) have been built and compared their performance in terms of Accuracy, Sensitivity, Specificity and Precision using 10 fold cross validation.
	      
For the campaigning need organizations rely mainly on either mass campaign, which targets a larger population or direct campaign, which targets specific clients. Formal studies have shown mass campaign to be as less effective as 1% positive response. In contrast, the direct campaign focuses on specific potential clients and is often data driven, and more effective.
In the age of Big-Data, it is nearly impossible to scale without data-driven techniques and solutions. The bank in the question is considering how to optimize this campaign in future. We can make data-driven decision to suggest marketing manager about effective client selection, which would increase the conversion rate. Direct marketing is effective yet it has some drawbacks, such as causing negative attitude towards banks due to the intrusion of privacy. It would be interesting to investigate how we can decrease the outbound call rate and use inbound calls for cross-selling intelligently to increase the duration of the call. We will discuss later why the duration of a call is an important parameter here.


We will be building few classifiers to predict whether a particular client will subscribe to term deposit or not. If classifier has very high accuracy it can help the manager to filter clients and use available resources more efficiently to achieve the campaign goal. Besides, we would also try to find influential factors for decision so that we can establish efficient and precise campaigning strategy. Proper strategy would reduce cost and improve long term relations with the clients.



Data description:
Initial Impression about data 
•	Let's discuss about initial impression that can be created using the dataset 
•	Total 45211 records 
•	7 numeric attributes: age, balance, day, duration, campaign, pdays, previous 
•	10 Factors: 
•	6 multi-valued categorical attributes: job, marital, education, contact, month, poutcome 
•	3 yes/no binary attributes: default, housing, loan 
•	1 target attribute y 
•	No missing values: Preprocessing should be easier 

