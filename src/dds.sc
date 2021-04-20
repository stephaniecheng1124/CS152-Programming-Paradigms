object dds {

	//Problem 1
	
	//Find a tail recursive implementation of controlLoop.
	//A tail recursive control loop shows us that it's possible to model
	//a state-changing system (such as a computer) without using variables!
	
	def controlLoop[S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S, Int)=>S): S =
   	if(halt(state,cycle)) state
		else controlLoop(update(state, cycle), cycle + 1, halt, update)
                                                  //> controlLoop: [S](state: S, cycle: Int, halt: (S, Int) => Boolean, update: (S
                                                  //| , Int) => S)S

	//Problem 2
	
	//A pond of amoebas reproduces asexually until the pond's carrying capacity is reached.
	//More specifically, the initial population of one doubles every week until it exceeds 10^5.
	//Use your controlLoop function to compute the size of the final population.

	def populationUpdate(currentPop: Int, time: Int) = {
		println("population = " + currentPop)
		currentPop * 2
	}                                         //> populationUpdate: (currentPop: Int, time: Int)Int
	def endPopulation(currentPop: Int, time: Int) =
		currentPop <= 1 || currentPop > Math.pow(10,5)
                                                  //> endPopulation: (currentPop: Int, time: Int)Boolean

	controlLoop[Int](2, 0, endPopulation _, populationUpdate _)
                                                  //> population = 2
                                                  //| population = 4
                                                  //| population = 8
                                                  //| population = 16
                                                  //| population = 32
                                                  //| population = 64
                                                  //| population = 128
                                                  //| population = 256
                                                  //| population = 512
                                                  //| population = 1024
                                                  //| population = 2048
                                                  //| population = 4096
                                                  //| population = 8192
                                                  //| population = 16384
                                                  //| population = 32768
                                                  //| population = 65536
                                                  //| res0: Int = 131072

	//Problem 3
	
	//r where |f(r)| <= delta
	def solve(f: Double=>Double) = {
	
		val delta = 1e-5
		def df(x: Double) = (f(x + delta) - f(x))/delta
		def improve(guess: Double, cycle: Int) = guess - f(guess)/df(guess)
		def goodEnough(guess: Double, cycle: Int) = math.abs(f(guess)) <= delta
	
		controlLoop[Double] (1.0, 0, goodEnough, improve)
	
	}                                         //> solve: (f: Double => Double)Double
	
	//Problem 4:
	//Note: roots of x^2 - n would be -n and +n
	def squareRoot(n: Double) = solve((x: Double) => x * x - n)
                                                  //> squareRoot: (n: Double)Double
	squareRoot(49)                            //> res1: Double = 7.000000142285558
	squareRoot(64)                            //> res2: Double = 8.000000000001208
	squareRoot(4)                             //> res3: Double = 2.0000000944796694
	
	
	//Problem 5:
	def cubeRoot(n: Double) = solve((x: Double) => x * x * x - n)
                                                  //> cubeRoot: (n: Double)Double
	cubeRoot(8)                               //> res4: Double = 2.000000000036784
	cubeRoot(27)                              //> res5: Double = 3.0000000000019176
	cubeRoot(64)                              //> res6: Double = 4.000000000119973
	
	//Problem 6:
	def nthRoot(x: Double, n: Int) = solve((d: Double) => Math.pow(d, n) - x)
                                                  //> nthRoot: (x: Double, n: Int)Double
  
	nthRoot(49, 2)                            //> res7: Double = 7.000000142285558
	nthRoot(27, 3)                            //> res8: Double = 3.000000000001917
	nthRoot(16, 4)                            //> res9: Double = 2.0000000000151514


}