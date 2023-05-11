import xxhash

# generate a 32-bit hash of a string
text = 'a'
data = bytes(text, encoding='utf-8');
print(f'Text: {text}')
hash64 = xxhash.xxh64(data, seed=0)
print(f'{hash64.intdigest()} = {hash64.hexdigest()}')

# generate a 64-bit hash of a file
filename = 'README.md' 
with open(filename, 'rb') as f:
    print(f'File: {filename}')
    hash64 = xxhash.xxh64(f.read(), seed=0)
    print(f'{hash64.intdigest()} = {hash64.hexdigest()}')
    
    
filename = 'c:/temp/v08.mp4' 
with open(filename, 'rb') as f:
    print(f'File: {filename}')
    hash64 = xxhash.xxh64(f.read(), seed=0)
    print(f'{hash64.intdigest()} = {hash64.hexdigest()}')    

