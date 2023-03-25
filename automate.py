import os
# assign directory
directory = 'testcases'
 
# iterate over files in
# that directory

printMain = '\\n";\n'
print1 = 'print "AST===========>\\n";\n'
text1 = '(PrintAbsyn.print(TextIO.stdOut, Parse.parse "'
text1_2 = '"));\n'
print2 = 'print "TYPECHECKING=============>\\n";\n'
text2 = 'Main.compile "'
text2_2 = '";\n'


# Append-adds at last
file1 = open("printTests.txt", "w") # append mode
file1.write('CM.make "ir_translation/sources.cm";\n')
for filename in os.listdir(directory):
    if ("test" in filename and "sara" not in filename):
        f = os.path.join(directory, filename)
        # checking if it is a file
        if os.path.isfile(f):
            file1.write('print "'+ str(f) + printMain)
            #file1.write(print1)
            f#ile1.write(text1 + str(f) + text1_2)
            file1.write(print2)
            file1.write(text2 + str(f) + text2_2)




file1.close()

