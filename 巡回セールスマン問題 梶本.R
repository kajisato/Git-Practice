# 順序組み換え
cross <- function(v1,v2) {

			l <- length(v1)
			cut_point <- sample(1:(l-1),1)

			rest_v1 <- v1 [ (cut_point+1) : l ]
			rest_v2 <- v2 [ (cut_point+1) : l ]

			junjo2 <- is.element( v2 , rest_v1 )
			junjo1 <- is.element( v1 , rest_v2 )

			junjo2 <- v2[ which( junjo2 == TRUE ) ]
			junjo1 <- v1[ which( junjo1 == TRUE ) ]

			v1 [ (cut_point+1) : l ] <- junjo2
			v2 [ (cut_point+1) : l ] <- junjo1

			return( matrix( c(v1,v2) , l , 2 ) )
			}

cross(c(1,4,3,2,6,5),c(2,1,3,5,4,6))

#逆位
mutation_reverse <- function(v) {

			l <- length(v)

			reverse_point <- sort( sample(1:l,2) )

			v[reverse_point] <- rev( v[reverse_point] )

			return(v)
				}

#選抜
selection <- function( now_gen , T ) {
			N <- length( now_gen[1,] )

			fitness_gen <- apply( now_gen , 2 , fitness )
			survive <- order( order( fitness_gen ) )
			survive <- which( survive >= N - T + 1 )

			return( now_gen[ , survive ] )

					}



# M : 親の数  T : 淘汰されて残る数  p : 突然変異率

generation <- function( now_gen , M , T , p) {

			N <- length( now_gen[1,] )

			parent <- sample(1:N , M ,replace=TRUE)
			parent_gen <- now_gen[ , parent ]

			parent_gen <- cbind( parent_gen , parent_gen[,1] )

			for( i in 1:M ) {

			# 変位
				child_gen <- cross( parent_gen[,i] , parent_gen[,i+1] )
			# 突然変異
				mutation_judge <- sample( c(0,1) , 2 , replace=TRUE , pro = c(1-p,p) )

					if( mutation_judge[1] == 1 ) {
							child_gen[,1] <- mutation_reverse ( child_gen[,1] )
									}

					if( mutation_judge[1] == 2 ) {
							child_gen[,2] <- mutation_reverse ( child_gen[,2] )
									}

				now_gen <- cbind( now_gen , child_gen )

					}

			now_gen <- selection( now_gen,T )
			return( now_gen )
						}


n <- 50 ; M <- 250 ; T <- 200 ; p <- 0.03

# n : 都市数  city : n都市の座標の位置行列 (n*2行列) 、Dcity : 都市間の距離行列 (n*n行列)
city <- matrix ( runif(n*2) , n , 2 )
city <- w * city
plot(city)

Dcity <- matrix( rep(0,n^2) , n , n )

for ( i in 1:n ){
	for( j in 1:n ) {
			kyori <- sqrt ( sum ( ( as.vector ( city[i,] - city[j,] ) )^2 ) )
			Dcity [i,j] <- kyori
			Dcity [j,i] <- kyori
			}
		}


# Dcityは引数にしてません。
walk_d <- function(v) {
			l <- length(v)
			distance <- rep(0, l-1 )

			for( i in 1:(l-1) ) {
					distance[i] <- Dcity[ v[i] , v[i+1] ]
						}
			return( sum(distance) )
			}

fitness <- function(v) {
			1 / walk_d (v)
			}

# 個体集合に対して、その中で最も距離の短い都市順番ベクトルをかえす

min_walk <- function( now_gen ) {
			distance_vec <- apply ( now_gen , 2 , walk_d )

			anw <- now_gen [ , which( distance_vec == min(distance_vec) ) ]

			if( is.matrix(anw) ) { anw <- anw[,1] }

			return( list( anw , walk_d( anw ) ) )
				}

# 都市順番ベクトルに対して、plotする関数
# Dcity や city は引数にしてません。
walk_plot <- function(v) {
			city2 <- city [ v , ]

			plot(city)

			par(new=T)
			lines( city2[,1] , city2[,2] )
			}
			
			

# M : 親の数  T : 淘汰されて残る数 (初期の遺伝子数)  ( n : 都市の数 )  

now_gen <- matrix( rep( 0 , n*T ) , n , T )

for( i in 1:T ) {
		now_gen[ ,i ] <- sample( 1:n )
		}


generation( now_gen , M , T , p)

exm_before <- min_walk(now_gen)
walk_plot( exm_before[[1]] )

exm_before

for( i in 1:1000) {
		now_gen <- generation( now_gen , M , T , p )

		if( i %% 10 == 0 ) {
				walk_plot( min_walk( now_gen )[[1]] )
				    }
		}

exm_after <- min_walk(now_gen)
walk_plot( exm_after[[1]] )

exm_after
