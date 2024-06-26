# Matrix Multiplication in Haskell using the ST Monad

This is a simple three loop implementation of row major order matrix matrix multiplication. 
Employs mutable state via the ST Monad.
Uses unsafe (without bound checks) array read and write. 
The relevant code is in app/MatMul.hs.
Runs around the speed of the [same algorithm implemented in safe rust](https://github.com/jd11111/mat-mul-rs) (so pretty fast).

## Usage:
Run ```cabal bench``` to get the time it takes to multiply two random 500x500 matrices.

## How it works:
Indexing starts at 0 here.
The following is a brief overview of the employed algorithm.

### Storing matrices in a vector
Storage of a $n \times m$ matrix in a length $n \cdot m$ vector:
- **Column major order**: Stack the columns of the matrix ontop of each other. Denoted by $c$.
- **Row major order**: Stack the rows ontop of each other. Denoted by $r$.

For example:
Let
```math
A := 
\begin{pmatrix}
a_{00} & a_{01} & a_{02} \\
a_{10} & a_{11} & a_{12} \\
a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{3 \times 3}
```
Then $A$ stored in column major order will be:
```math
c \ A := 
\begin{pmatrix}
a_{00} & a_{10} & a_{20} & a_{01} & a_{11} & a_{21} & a_{02} & a_{12} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{9}
```
and $A$ stored in row major order will be:
```math
r \ A := 
\begin{pmatrix}
a_{00} & a_{01} & a_{02} & a_{10} & a_{11} & a_{12} & a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{9}
```
The interesting relation $r \ A^t = c \ A$ (and vice versa) is true (here $t$ denotes the transpose).

## Storing Complex Matrices
First do row major order, then
store real part and imaginary part next to each other.
```math
A := 
\begin{pmatrix}
a_{00} & a_{01} & a_{02} \\
a_{10} & a_{11} & a_{12} \\
a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{3 \times 3}
```
and $A$ stored in row major order will be:
```math
r \ A := 
\begin{pmatrix}
\Re a_{00} & \Im a_{00}  & \Re a_{01} & \Im a_{01} & \Re a_{02} &\Im a_{02}  & a_{10} & a_{11} & a_{12} & a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{2 \cdot 9}
```


### Retrieving value in stored vector by matrix indices

Going from matrix to column major storage:  
Define $I_c : \\{0,\dots, n-1\\} \times \\{0, \dots, m-1   \\} \to \\{0, \dots, n \cdot m -1\\} $
by $I_c(i,j) =i + n \cdot j$.  
Then $A(i,j) = c \ A \ (I_c (i,j))$ for all $(i,j)$.

Going from matrix to row major storage:  
Define $I_r : \\{0,\dots, n-1\\} \times \\{0, \dots, m-1\\} \to \\{0, \dots, n \cdot m -1\\} $
by $I_r(i,j) =m \cdot i +  j$.  
Then $A(i,j) = r \ A \ (I_c (i,j))$ for all $(i,j)$.

### Matrix Multiplication

Let $A \in \mathbb{R}^{N \times M}$ and $B \in \mathbb{R}^{M \times K}$.
Then $C := A \cdot B \in \mathbb{R}^{N \times K}$ is given by
$$C (i,j) := \sum_{k=0}^{M-1} A(i,k) \cdot B(k,j).$$
To calculate $C(i,j)$ we need to access the $i$-th row of $A$ and the $j$-th column of $B$.
We want these values to be cached on the CPU. Therefore
all the values of the $i$-th row of $A$ and the $j$-th column of $B$ need to be close together in memory.
This is achieved if $A$ is stored in row major order and $B$ in column major order.  
It should not make a big difference, as long as the matrices are small.  
For now this is not implemented in the code.
