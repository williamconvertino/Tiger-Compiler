import subprocess
import re

tiger_files = subprocess.check_output(["ls ../testcases/*.tig"], shell=True, text=True)

errored_tests = []

for testfile in tiger_files.split('\n')[:-1]:
    print("---\ntest: ", testfile)

    try:
        sml_output = subprocess.check_output(["sml", "tester.sml", testfile], text=True)
        trimmed_out = re.split("FINISHING TEST\n", re.split('STARTING TEST\n', sml_output)[1])[0]

        print(trimmed_out)
    except:
        errored_tests.append(testfile)
        print('sml compiler error')
    print('---')

print('These tests raised an sml error: ', errored_tests)