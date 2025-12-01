def read_file(path):
    with open(path, 'r') as f:
        return [line.strip() for line in f if line.strip()]

pos = 50

# Part 1: número de rotaciones que terminan exactamente en 0
ends_on_zero = 0

# Part 2: número de veces que cualquier click hace que el dial apunte a 0
clicks_on_zero = 0

for op in read_file("inputs/input1.txt"):
    dirc = op[0]
    d = int(op[1:])

    if dirc == "R":
        # al moverse a la derecha, hits = floor((pos + d) / 100)
        hits = (pos + d) // 100
        clicks_on_zero += hits
        pos = (pos + d) % 100

    else:  # "L"
        if pos == 0:
            # primer 0 aparece en k=100
            hits = d // 100
        else:
            if d >= pos:
                hits = (d - pos) // 100 + 1
            else:
                hits = 0
        clicks_on_zero += hits
        pos = (pos - d) % 100

    if pos == 0:
        ends_on_zero += 1

print("Part 1 (termina en 0):", ends_on_zero)
print("Part 2 (cualquier click en 0):", clicks_on_zero)
