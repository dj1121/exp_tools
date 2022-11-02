import re

def fix_item_numbers(lines):
    to_return = []
    item_counter = 1

    for i in range(len(lines)):
        curr_line = lines[i]

        # Get item number
        digit_pos = -1
        rb_pos = curr_line.find("]")
        for k, c in enumerate(curr_line):
             if c.isdigit():
                digit_pos = k
                break

        s = list(curr_line)
        s_prev = s[0:digit_pos]
        s_dig = list(str(item_counter))
        s_next = s[rb_pos:]
        

        # Rebuild string with fixed item number
        fixed = "".join(s_prev + s_dig + s_next)

        to_return.append(fixed)

        if (i+1) % 4 == 0:
            item_counter += 1

    return to_return

def check_sentences(lines):
    for line in lines:
        
        line = " ".join(line.split())

        # Check if extra spaces
        if "  " in line:
            print("Extra space found:")
            print(line)

        s = line.find("s:")
        a = line.find("a:")

        real = line[s:a].split()
        distractor = line[a:].split()

        # Check number of spaces match
        c_real=0
        c_distractor=0
        for i in line[s:a].strip():
            if(i.isspace()):
                c_real+=1
        for i in line[a:-1].strip():
            if (i.isspace()):
                c_distractor += 1
        
        if c_real != c_distractor:
            print("Number of spaces mismatched")
            print(real)
            print(distractor)
         

        # Check that the lengths of real word sentences are same as distractors
        if len(real) != len(distractor):
            print("Error: lines not same length in words. FIX MANUALLY PLEASE!")
            print(real)
            print(distractor)

        merged_pseudo = " ".join(distractor)
        merged_real = " ".join(real)

        x = re.sub(r"\s*[\r\n]\s*", r" \r ", merged_pseudo).split(r"[ \t]+")
        y = re.sub(r"\s*[\r\n]\s*", r" \r ", merged_real).split(r"[ \t]+")
        if len(x) != len(y):
            print("Error: lines not same length in words. FIX MANUALLY PLEASE!")
            print(x)
            print(y)

        
with open("items_ibex.js", "r") as f:

    lines = f.readlines()
    lines = fix_item_numbers(lines)
    check_sentences(lines)

    # with open("fixed_items.js", "w") as m:
    #     for line in lines:
    #         m.write(line)

    
    
        