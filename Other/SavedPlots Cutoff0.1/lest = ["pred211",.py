lest = ["pred211",
"pred212",
"pred221",
"pred231",
"pred241",
"pred251",
"pred252",
"pred253",
"pred261",
"pred262",
"pred263",
"pred264",
"pred271",
"pred281",
"pred282",
"pred291",
"pred301"]
for i in range(0, 17):
    print("roc.perf", i+1, "=", "performance(", lest[i],",measure = 'tpr', x.measure = 'fpr')")