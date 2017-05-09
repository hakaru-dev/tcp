
def score(filename):
    with open(filename) as fh:
        correct, total = 0, 0
        fh.next()  # throw away header
        for line in fh:
            true, pred = map(int, line.split(','))
            total += 1
            if true == pred:
                correct += 1
    return (correct, total)


if __name__ == '__main__':
    for n in range(1, 101):
        print(score('nb-confusion-%s.csv' % n))
