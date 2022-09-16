ORDER RUNNING FILE AND SUMMARY:
1) aida_preparation.R (create 3 files aida_active_finale.RData, aida_failed_finale.RData, aida_finale.RData, 16 GB RAM highly recommanded to save aida_finale.RData)
2) questionA.r, questionB.r, questionC.r (answer question A, B, C)
3) test_train_prepartion.r (create 2 files: aida_train.RData, aida_test.RData)
5) questionD1(2).r (answer question D logistic regression (Random Forest), stepAIC and rcv could need some time to perform, in our test using Intel(R) Core(TM) i7-7700HQ CPU @ 2.80GHz 2.81 GHz it needs between 15-30 minutes to performs)
6) questionE_data_preparation.r (create 2 files: aida_train_e.RData, aida_test_e.RData)
7) questionE.r (Random Forest and Logistic Regression using test and train ROSE=
8) questionE_test.r (applying ROSE RF model to test used in question D)

-useful_functions.r (functions created and used for this project)
There could be some difference in results due to a different seed  used in  test_train_prepartion.r.

Manuel Luci, Carlo Volpe, Salvatore Fergola