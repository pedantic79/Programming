#!/usr/bin/python3

def flip(num):

    if (num < 0):
        num = num * -1
        negative = True
    else:
        negative = False

    out = 0
    while num != 0:
        digit = num % 10
        num = (num - digit) / 10
        out = out * 10 + digit

    if negative:
        out = out * -1

    return int(out)


print(flip(1234))
print(flip(-1234))
