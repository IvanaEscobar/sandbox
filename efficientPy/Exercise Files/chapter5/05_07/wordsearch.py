import gzip
import multiprocessing

words = [line.strip() for line in gzip.open('words.gz','rt')]

target = 'zygomaticum'

chunksize = 16384

def worker(i):
    try:
        return i + words[i:i+chunksize].index(target)
    except ValueError:
        return None
    
if __name__ == '__main__':
    pool = multiprocessing.Pool(processes=4)
    results = pool.map(worker,range(0,len(words),chunksize))
    pool.close()
    
    print([r for r in results if r is not None])
    print(words[235786])
