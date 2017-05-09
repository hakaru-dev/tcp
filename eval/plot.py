import matplotlib.pyplot as plt
from csv import DictReader


def data_from(filename):
    with open(filename) as csvfile:
        reader = DictReader(csvfile)
        return [{k: float(v) for (k, v) in row.items()} for row in reader]


if __name__ == '__main__':
    data = data_from('scores.csv')
    docs = [row['documents'] for row in data]
    times = [(row['user_time'] + row['sys_time']) / row['total'] for row in data]
    scores = [row['correct'] / row['total'] for row in data]
    plt.plot(docs, times, 'ro', alpha=0.5, label='Run time (seconds) per document')
    plt.plot(docs, scores, 'bo', alpha=0.5, label='Accuracy = correct / total')
    plt.xlabel('Sampled documents per topic')
    plt.grid(True)
    plt.legend()
    plt.show()
