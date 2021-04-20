//Problem 5: Weather station

//output of WeatherStation tester:
// avg temp = 26.079666216941884


trait IThermometer {
   // = avg degrees Farenheit
   def getMeanTemperature(cities: List[String]): Double
}

class CelsiusTherm {
   // = degrees Celsius 
   def computeTemp(city: String) = 50 * math.random // fake temperature for now
}

class Adaptee extends CelsiusTherm {

  def avg(nums: List[Double]): Double = {
    if (nums.length == 0) throw new Exception("length = 0")
    var sum = 0.0
    for(i <- nums) sum += i
    sum / nums.length
    }
  
	def meanMapReduce(cities: List[String]): Double = {
	  avg(cities.map(computeTemp _))
	}
	
}

class ThermAdapter extends Adaptee with IThermometer {
   def getMeanTemperature(cities: List[String]): Double = {
     meanMapReduce(cities) 
   }
  

  
}
