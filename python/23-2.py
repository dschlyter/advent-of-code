test = False
if test:
    b = 93
    c = 93
else:
    b = 109300
    c = 126300

h = 0
# mc = 0


while True:
    f = 1
    d = 2
    # progress
    print (c - b) / 17
    while True:
        e = 2
        # opt
        if b > e*d and b % d == 0:
            f = 0
        # while True:
            # g = d
            # g = g * e
            # g = g - b
            # if g == 0:
                # f = 0
            # e = e + 1
            # g = e
            # g = g - b
            # if g == 0: break
        d = d + 1
        g = d
        g = g - b
        if g == 0: break
    if f == 0:
        h = h + 1
    g = b
    g = g - c
    if g == 0 or False:
        break
    b = b + 17

print "h is", h
# print "mc is", mc
