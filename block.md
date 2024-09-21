# Block
## 區塊鏈是什麼？
http://users.dimi.uniud.it/~massimo.franceschet/HEX0x6C/blockchain/blockchainR.html


### 區塊（blocks）
區塊是儲存資料的容器，就是將交易資料記錄在區塊中，像是付款人是誰、受款人是誰與款項為多少…等。只要有新的交易就會產生新的資料、誕生出新的區塊，而這個新的區塊便會與其他既有的區塊，鏈結在一起. 让我们来看看区块链如何利用密码学变得几乎不可破解。

在 R 語言中我們可以使用 list 這個型別作為儲存資料的區塊
```{r}
block <- list(number = 3, # an identification number
             timestamp = "2018-10-01 17:24:18 CEST", # timestamp of block creation
             data = "London", # a bunch of data
             parent = 2) # a reference to the previous block (parent block) in the chain
block
```
### Chain
Blocks are concatenated into a chain. The first block of the chain is called the genesis block and has no parent block. The last block of a chain is the current block and is not parent of any other block. 
```{r}
# some blocks
block1 <- list(number = 1,
             timestamp = "2018-10-01 17:24:00 CEST",
             data = "London",
             parent = NA) # first block 

block2 <- list(number = 2,
             timestamp = "2018-10-01 17:24:15 CEST",
             data = "Paris",
             parent = 1)

block3 <- list(number = 3,
             timestamp = "2018-10-01 17:24:30 CEST",
             data = "Rome",
             parent = 2) # last block

# the blockchain
blockchain = list(block1, block2, block3)

# get the 2nd block
blockchain[[2]]
```
### validation 
Let’s also write a validation function for the blockchain.
```{r}
validate = function(blockchain) {
  if (length(blockchain) >= 2) {
    for (i in 2:length(blockchain)) {
      if (blockchain[[i]]$parent != blockchain[[i-1]]$number) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

validate(blockchain)
```

### Hash
Hashes of blocks are created using cryptographic hash functions, that are mathematical algorithms that maps data of arbitrary size to a bit string of a fixed size (called hash or digest).
Hash 可以將一個文字轉換成一個獨特的字串，在 R 語言中我們可以透過 digest 套件的 digest() 函數獲得 Hash 的能力
```{r}
library("digest")
digest("R", "sha256") # 把r and sha256 變成字串
digest(c("R", "Python"), "sha256")
```

```{r}
# hash blocks
block1 <- list(number = 1,
             timestamp = "2018-10-01 17:24:00 CEST",
             data = "London",
             parent_hash = "0")

block1$hash = digest(block1, "sha256")

block2 <- list(number = 2,
             timestamp = "2018-10-01 17:24:15 CEST",
             data = "Paris",
             parent_hash = block1$hash)

block2$hash = digest(block2, "sha256")

block3 <- list(number = 3,
             timestamp = "2018-10-01 17:24:30 CEST",
             data = "Rome",
             parent_hash = block2$hash)

block3$hash = digest(block3, "sha256")

# the blockchain
blockchain = list(block1, block2, block3)
```
Let’s update the validation function 

```{r}
validate = function(blockchain) {
  for (i in 1:length(blockchain)) {
    block = blockchain[[i]]
    hash = block$hash
    block$hash = NULL
    hash_expected = digest(block, "sha256")
    if (hash != hash_expected) {
      return(FALSE)
    }
  }
  if (length(blockchain) >= 2) {
    for (i in 2:length(blockchain)) {
      if (blockchain[[i]]$parent_hash != blockchain[[i-1]]$hash) {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

validate(blockchain)
```
Alter data of first block
```{r}
blockchain[[1]]$data = "Budapest"
validate(blockchain)
```
the answer is FALSE

## Proof-of-Work
Hash alone is not enough to prevent tampering, since hash values can be computed fast by computers. A Proof-of-Work (PoW) algorithm controls **the difficulty** of creating a new block.

Now let’s build a blockchain using the PoW method.











接著建立一個函數 get_hashed_block()，這個函數的作用是將儲存資料的區塊加密，把區塊的索引、時間戳記、資料與前一個區塊**的 Hash 一併也**加密的動作造就了鏈結的效果
```{r}
get_hashed_block <- function(block){
  to_be_hashed <- c(
                   block$index,
                   block$timestamp,
                   block$data,
                   block$previous_hash
  )
  block$new_hash <- digest(to_be_hashed, "sha256")
  return(block) # 利用sha256 加密 
}
```

### 工作量證明 Proof-Of-Work (POW)
證明你做了一定量的工作，可用工作成果來證明完成相應的工作量, 
工作量證明（POW）共識機制算法採用了SHA-256 運算 Hash 值，特點是難以運算，卻容易驗證. 工作量證明（POW）共識機制算法採用了SHA-256 運算 Hash 值，特點是難以運算，卻容易驗證。

改變 Hash 值的原始數據中的任何一部份，所產生的 Hash值也會隨之變化，因此我們只需在運算 Hash 值時，加入一個不斷改變的隨機數.所以要找到符合要求的區塊 Hash 值，則需要經過進行大量運算.

SHA-256生成的 Hash值都為256 位元，Hash值是由數字和大小寫字母構成的字符串，每一位有62種可能性. 假設任何一個字符出現的概率是均等的，那麼第一位為0的概率是1/62，理論上需要嘗試 62次Hash運算才會出現一次第一位為0的情況，如果前兩2位為0，就得嘗試62的平方次Hash運算，以N個0開頭就需要嘗試62的N次方次運算. 最新的區塊顯示Hash值有18個0開頭，理論上需要嘗試62的18次方次，這個數字是非常龐大的。如此大的運算量需要投入大量的運算設備、電力.

### 建立新區塊
```{r}
#A function that takes the previous block and normally some data (in our case the data is a string indicating which block in the chain it is)
gen_new_block <- function(previous_block){
  #Proof-of-Work
  new_proof <- proof_of_work(previous_block$proof)
  
  #Create new Block
  new_block <- list(index = previous_block$index + 1,
                    timestamp = Sys.time(),
                    data = paste0("this is block ", previous_block$index +1),
                    previous_hash = previous_block$new_hash,
                    proof = new_proof)
  
  #Hash the new Block
  new_block_hashed <- hash_block(new_block)
  return(new_block_hashed)
}
```

在开始构建区块链之前，您需要在某处启动链。 这是通过使用所谓的创世块完成的。 它不包含任何数据和用于证明和以前散列的任意值（因为没有以前的块）。
```{r}
# Define Genesis Block (index 1 and arbitrary previous hash)
block_genesis <-  list(index = 1,
                       timestamp = Sys.time(),
                       data = "Genesis Block",
                       previous_hash = "0",
                       proof = 1)
```

### 构建区块链

```{r}
blockchain <- list(block_genesis)
    previous_block <- blockchain[[1]]
      
# How many blocks should we add to the chain after the genesis block
      num_of_blocks_to_add <- 5
      
# Add blocks to the chain
      for (i in 1: num_of_blocks_to_add){
        block_to_add <- gen_new_block(previous_block) 
        blockchain[i+1] <- list(block_to_add)
        previous_block <- block_to_add
        print(cat(paste0("Block ", block_to_add$index, " has been added", "\n",
                   "\t", "Proof: ", block_to_add$proof, "\n",
                   "\t", "Hash: ", block_to_add$new_hash)))
      }
```
