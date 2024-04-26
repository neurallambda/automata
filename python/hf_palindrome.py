'''

Prepare the `palindrome` dataset as an HF dataset

'''

import os
import json
from datasets import Dataset, DatasetDict, concatenate_datasets
import random

N_SWAPS = 2
MAX_LENGTH = 25  # max sentence length (after each char has been separated with space)
HF_TOKEN = os.getenv('HF_TOKEN')

TRAIN_N = 5000 # truncate datasets
TEST_N = 100 # truncate datasets

train_path = os.path.expanduser("~/_/neurallambda-automata/data/palindrome_progs.json")
test_path = os.path.expanduser("~/_/neurallambda-automata/data/palindrome_2_progs.json")


##########
# Util

# Make out-of-class versions of palindromes
def swap_chars(x, n, give_up_after=10):
    '''Swap characters in x, n times. Ensure the result is different than the
    input, or else return None.'''
    for _ in range(give_up_after): # try 10 times before giving up
        cs = list(x)
        for _ in range(n):
            i, j = random.sample(range(len(x)), 2)
            cs[i], cs[j] = cs[j], cs[i]
        out = ''.join(cs)
        if not out == x:
            return out
    return None


##########
#

def load_data(path):
    # load data
    with open(path, 'r') as f:
        data = json.load(f)

    raw_sentences = data['sentences']
    spec = data['spec']   # dict_keys(['machine', 'rules', 'symbols'])

    noised_sentences = map(lambda x: swap_chars(x, n=N_SWAPS), raw_sentences)

    data_list = (
        [{'text': x, 'label': 'T'} for x in raw_sentences if x is not None] +
        [{'text': x, 'label': 'F'} for x in noised_sentences if x is not None]
    )

    # separate chars with spaces (into separate tokens)
    for x in data_list:
        x['text'] = ' '.join(list(x['text']))

    dataset = Dataset.from_list(data_list)

    # truncate long sentences
    dataset = dataset.filter(lambda x: len(x["text"]) <= MAX_LENGTH)
    return dataset

dataset_dict = DatasetDict({
    'train': load_data(train_path),
    'test': load_data(test_path),
})

# truncate test set
random_indices = random.sample(
    range(len(dataset_dict['test'])),
    min(TEST_N if TEST_N else float('inf'), len(dataset_dict['test'])))
dataset_dict['test'] = dataset_dict['test'].select(random_indices)

random_indices = random.sample(
    range(len(dataset_dict['train'])),
    min(TRAIN_N if TRAIN_N else float('inf'), len(dataset_dict['train'])))
dataset_dict['train'] = dataset_dict['train'].select(random_indices)

# randomize the order of examples in each split
dataset_dict = dataset_dict.shuffle(seed=42)

# sort by length
dataset_dict = dataset_dict.map(lambda x: {'length': len(x['text'])})
dataset_dict = dataset_dict.sort('length')
dataset_dict = dataset_dict.remove_columns('length')

# Sample
print(f"Examples from Training: {len(dataset_dict['train'])} total rows")
for example in dataset_dict['train']['text'][:5] + dataset_dict['train']['text'][-5:]:
    print(example)

print(f"Examples from Testing: {len(dataset_dict['test'])}")
for example in dataset_dict['test']['text'][:5] + dataset_dict['test']['text'][-5:]:
    print(example)


# save the dataset to disk
dataset_dict.save_to_disk('palindrome_dataset')

print('pushing to huggingface')
if HF_TOKEN is not None:
    resp = input('pushing data up to huggingface, ok?')
    if resp == 'y' or resp == 'yes':
        dataset_dict.push_to_hub('neurallambda/automata_palindrome', token=HF_TOKEN)
    else:
        print('permission denied')
else:
    print('HF_TOKEN not found in env, skipping pushing to hub')
