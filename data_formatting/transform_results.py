with open("results.csv", "r") as f:
    lines = f.readlines()
    lines_wanted = []

    pid = None
    for line in lines:
        if "PennController" in line:
            s = line.split(",")
            pid = s[-2]
            continue

        if "#" not in line:
            s = line.split(",")
            wanted = ",".join(s[5:9] + s[11:14])
            wanted = pid + "," + wanted
            lines_wanted.append(wanted)
        
    with open("results_fixed.csv", "w") as w:
        w.write("subj,cond,item_num,word_num,word,correct,rt,sentence\n")
        for line in lines_wanted:
            w.write(line + "\n")
