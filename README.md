## Matrix Multiplication

Indexing starts at 0 here.

## Storing matrices in a vector
Storage of a $n \times m$ matrix in a length $n \cdot m$ vector:
- **Column major order**: Stack the columns of the matrix ontop of each other. Denoted by $c$.
- **Row major order**: Stack the rows ontop of each other. Denoted by $r$.

For example:
Let
$$A := 
\begin{pmatrix}
a_{00} & a_{01} & a_{02} \\\
a_{10} & a_{11} & a_{12} \\\
a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{3 \times 3}$$
Then $A$ stored in column major order will be:
$$c \ A := 
\begin{pmatrix}
a_{00} & a_{10} & a_{20} & a_{01} & a_{11} & a_{21} & a_{02} & a_{12} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{9}$$
and $A$ stored in row major order will be:
$$r \ A := 
\begin{pmatrix}
a_{00} & a_{01} & a_{02} & a_{10} & a_{11} & a_{12} & a_{20} & a_{21} & a_{22}
 \end{pmatrix} \in  \mathbb{R}^{9}$$
The interesting relation $r \ A^t = c \ A$ (and vice versa) is true (here $t$ denotes the transpose).

## Retrieving value in stored vector by matrix indices

Going from matrix to column major storage:  
Define $I_c : \{0,\dots, n-1\} \times \{0, \dots, m-1\} \to \{0, \dots, n \cdot m -1\} $
by $I_c(i,j) =i + n \cdot j$.  
Then $A(i,j) = c \ A \ (I_c (i,j))$ for all $(i,j)$.

Going from matrix to row major storage:  
Define $I_r : \{0,\dots, n-1\} \times \{0, \dots, m-1\} \to \{0, \dots, n \cdot m -1\} $
by $I_r(i,j) =m \cdot i +  j$.  
Then $A(i,j) = r \ A \ (I_c (i,j))$ for all $(i,j)$.

## Matrix Multiplication

Let $A \in \mathbb{R}^{N \times M}$ and $B \in \mathbb{R}^{M \times K}$.
Then $C := A \cdot B \in \mathbb{R}^{N \times K}$ is given by
$$C (i,j) := \sum_{k=0}^{M-1} A(i,k) \cdot B(k,j).$$
To calculate $C(i,j)$ we need to access the $i$-th row of $A$ and the $j$-th column of $B$.
We want these values to be cached on the CPU. Therefore
all the values of the $i$-th row of $A$ and the $j$-th column of $B$ need to be close together in memory.
This is achieved if $A$ is stored in row major order and $B$ in column major order.
