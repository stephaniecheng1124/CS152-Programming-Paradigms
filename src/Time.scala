package time1
 //Problem 0: Time lab

 //Output of TestTime:
 //t1 = 3:45
 //t2 = 3:0
 //t1 < t2 = ()
 //t1 minutes since midnight = 225

 class Time(val hour: Int, val min: Int) {
  
  override def toString() = 
  {
    if (hour <=23 && min <=59 ) {
      hour + ":" + min
    }
    else {
      throw new IllegalArgumentException
      
    }     
  }
  
  
  def before(t2: Time) = {
    if (this.hour < t2.hour) true
    else if (this.hour == t2.hour) {
      if(this.min < t2.min) true
    }
    else false
  }
  

  def minutesSinceMidNight() = {
    val hoursToMin = this.hour * 60
    val totalMins = hoursToMin + this.min
    
    totalMins
  }
  
}

object Time{
    def apply(hour: Int, min: Int = 0) = new Time(hour, min)    
 }


