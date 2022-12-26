from multiprocessing import Pool
import requests
import random


def runner(_):
    pl = { "type": "DUT",   
        "PASS": True,
        "TP": "TP1",
        "LOT": "a1",
        "product": "superdupa",
        "load": "111111111"*100
        }

    s = requests.session()
    s.auth = ("admin", "admin")


    urls = [f"http://localhost:5984/proddat/ara{random.randint(0,100000000000)}" for _ in range(1000)]

    for url in urls:
        s.put(url, json=pl)
        #print(url)

if __name__ == '__main__':
    with Pool(5) as p:
        print(p.map(runner, [1]))