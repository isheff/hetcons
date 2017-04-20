#!/usr/bin/python3

thrift_filename = "hetcons.thrift"
explanation = """
The purpose of this script is to act as a primitive macro / wrapper for thrift.
Specifically, thrift const structs do not allow fields to be declared using
previously defined const structs. Instead, the programmer must copy and paste
the definition of the previous struct into the field of the new one. This script
automates that process for """ + thrift_filename + """, runs
thrift -r --gen hs """+thrift_filename+"""
and then returns """+thrift_filename+""" to its original form.
For debugging, the altered version fed to thrift is copied to
/tmp/2"""+thrift_filename
print(explanation)


import re
from os import system


# read original contents
with open(thrift_filename, "r") as thrift_file:
  original = thrift_file.read()

start_of_def = r'^const\s+'
type_of_def = r'\w+\s+'
name_of_def = r'(\w+)\s*'
definition = r'=((.|\n\s)*)'
const_definitions = re.compile(start_of_def + type_of_def + name_of_def + definition, re.MULTILINE)

thrift = original
defns = const_definitions.search(thrift)
while (defns != None):
  const = defns.group(1)
  value = defns.group(2)
  index = defns.end()
  thrift = thrift[:index] + (re.sub(const, "\n "+value+"\n ", thrift[index:]))
  defns = const_definitions.search(thrift, index)

with open(thrift_filename, "w") as thrift_file:
  thrift_file.write(thrift)
with open("/tmp/2"+thrift_filename, "w") as thrift_file:
  thrift_file.write(thrift)

print("thrift returned exit status "+str(
      system("thrift -r --gen hs "+thrift_filename)))

# Return file to its original state
with open(thrift_filename, "w") as thrift_file:
  thrift_file.write(original)
