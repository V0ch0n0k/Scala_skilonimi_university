import Skillonomy.{Adr, Burse, Human, Platform, Smart_Contract, Student, Teacher}

import scala.collection.mutable.ListBuffer
import scala.util.Random

object Skillonomy
{
  class Human(var Name: String, var Surname: String, var Age: Int, var Email: String, var Adr: Adr)
  {
    private var name: String = Name;
    private var surname: String = Surname;
    private var age: Int = Age;
    private var email: String = Email;
    private var adr: Adr = Adr;

    def Show(): String = s"Name: ${name}\nSurname: ${surname}\nAge: ${age}\nEmail: ${email}\n Address: ${adr.toString()}";

    def Name(Value: String) = {
      name = Value;
    }

    def Surname(Value: String) = {
      surname = Value;
    }

    def Age(Value: Int) = {
      age = Value;
    }

    def Email(Value: String) = {
      email = Value;
    }

    def Adr(Value: Adr) = {
      adr = Value;
    }

    def Info_Show(): Unit = {
      println("Name: " + name);
      println("Surname: " + surname);
      println("Age: " + age);
      println("Email: " + email);
      println("");
    }
  }

  class Adr(var Country: String, var City: String, var House: Int)
  {
    private var country: String = Country;
    private var city: String = City;
    private var house: Int = House;

    def Country(Value1: String): Unit = {
      country = Value1;
    }

    def City(Value2: String): Unit = {
      city = Value2;
    }

    def House(Value3: Int): Unit = {
      house = Value3;
    }

    def Print(): Unit = {
      println("Country: " + country);
      println("City: " + city);
      println("House: " + house);
      println("");
    }
  }

  class Student(Name:String, Surname:String, Age:Int, Email:String, Adr:Adr, var Login:String, var Password:String, var Balance:Double, var Fiat:Double, var Smartcontract:Smart_Contract) extends Human(Name, Surname, Age, Email, Adr) with Burse_Connect
  {
    private var login: String = Login;
    private var password: String = Password;
    private var balance: Double = Balance;
    private var fiat: Double = Fiat;
    private var smartcontract: Smart_Contract = Smartcontract;

    def Get_Login : String = login;

    def Set_Login(Value: String) =
    {
      login = Value;
    }

    def Get_Password : String = password;

    def Set_Password(Value: String) =
    {
      password = Value;
    }

    def Get_Balance : Double = balance;

    def Set_Balance(Value: Double, birzha: Burse, platform: Platform, CoursePrice: Int) =
    {
      balance = Value;
    }

    def Get_Fiat : Double = fiat;

    def Set_Fiat(Value: Double) =
    {
      fiat = Value;
    }

    def Get_Smartcontract : Smart_Contract = smartcontract;

    def Set_Smartcontract(newValue: Smart_Contract) =
    {
      smartcontract = newValue;
    }

    def Info_Show2(): Unit =
    {
      println("Name: " + Name);
      println("Surname: " + Surname);
      println("Age: " + Age);
      println("Email: " + Email);
      println("Login: " + login);
      println("Password: " + password);
      println("Balance: " + balance);
      println("Fiats: " + fiat);
      println("");
    }
  }


  trait Burse_Connect
  {
    def Sell(TokkenSell: Double, burse: Burse, balance: Double, fiat: Double):Tuple2[Double, Double] =
    {
      for(i <- 0 until TokkenSell.toInt)
      {
        var new_price = burse.Get_Price - 0.01;
        burse.Set_Price(new_price);
      }
      var new_balance_burse_tokens = burse.Get_Tokens + TokkenSell;
      burse.Set_Tokens(new_balance_burse_tokens);
      var new_balance_person_tokens = balance - TokkenSell;
      var new_balance_person_fiat = fiat + (burse.Get_Price * TokkenSell);
      var new_balance_birga_fiat = burse.Get_Fiat - (burse.Get_Price * TokkenSell);
      burse.Set_Fiat(new_balance_birga_fiat);
      return (new_balance_person_tokens, new_balance_person_fiat)
    }

    def Buy(NeedsTokens: Double, burse: Burse, balance: Double, fiat: Double): Tuple2[Double, Double] = {
      for (i <- 0 until NeedsTokens.toInt) {
        var new_price = burse.Get_Price + 0.01;
        burse.Set_Price(new_price);
      }
      var new_balance_burse_tokens = burse.Get_Tokens - NeedsTokens;
      burse.Set_Tokens(new_balance_burse_tokens);
      var new_balance_person_tokens = balance + NeedsTokens;
      var new_balance_person_fiat = fiat - (burse.Get_Price * NeedsTokens);
      var new_balance_burse_fiat = burse.Get_Tokens + (burse.Get_Price * NeedsTokens);
      burse.Set_Fiat(new_balance_burse_fiat);
      return (new_balance_person_tokens, new_balance_person_fiat)
    }
  }

  trait Platform_Connect
  {
    def give(platform: Platform, price: Double) {
      var Value1 = platform.Get_Tokens - price;
      platform.Set_Tokens(Value1);
    }

    def get(platform: Platform, CorsePrice: Int) {
      var Value2 = platform.Get_Tokens + (CorsePrice / 2);
      platform.Set_Tokens(Value2);
    }
  }


  class Burse
  {
    private var Count_of_tokens: Double = 100000;
    private var Count_of_fias: Double = 100000;
    private var Price: Double = 1.0;

    def Get_Tokens: Double = Count_of_tokens;

    def Set_Tokens(Value1: Double) = {
      Count_of_tokens = Value1;
    }

    def Get_Fiat: Double = Count_of_fias;

    def Set_Fiat(Value2: Double) = {
      Count_of_fias = Value2;
    }

    def Get_Price: Double = Price;

    def Set_Price(Value3: Double) = {
      Price = Value3;
    }

    def InfoShow(): Unit = {
      println("How much tokens: " + Count_of_tokens);
      println("How much fiats: " + Count_of_fias);
      println("Price of token: " + Price);
      println("");
    }
  }


  class Platform extends Burse_Connect
  {
    private var tokens: Double = 100000;
    private var fiat: Double = 100000;


    def Get_Tokens: Double = tokens;

    def Set_Tokens(Value: Double) = {
      tokens = Value;
    }

    def Get_Fiat: Double = fiat;

    def Set_Fiat(Value: Double) = {
      fiat = Value;
    }
  }

  class Smart_Contract(var Profit: Int, var Period: Int, var Percentage: Int)
  {
    private var profit: Int = Profit;
    private var period: Int = Period;
    private var percentage: Int = Percentage;

    def Profit(Value1: Int) = {
      profit = Value1;
    }


    def Period(Value2: Int) = {
      period = Value2;
    }


    def Percentage(Value3: Int) = {
      percentage = Value3;
    }

    def Print(): Unit = {
      println("Profit: " + profit + " UAH");
      println("Period: " + period + " days");
      println("Percentage: " + percentage + "%");
      println("");
    }
  }

  class Teacher(Name:String, Surname:String, Age:Int, Email:String, Adr:Adr, var Login:String, var Password:String, var Balance:Double, var Fiat:Double, var Smartcontract:Smart_Contract, var CoursePrice: Int) extends Human(Name, Surname, Age, Email, Adr) with Burse_Connect with Platform_Connect
  {
    private var login: String = Login;
    private var password: String = Password;
    private var balance: Double = Balance;
    private var fiat: Double = Fiat;
    private var smartcontract: Smart_Contract = Smartcontract;
    private var Student_list = new ListBuffer[Student]();
    private var Number_of_students: Int = Student_list.length;
    private var courseprice: Int = CoursePrice;

    def Add_in_List(student: Student): Unit =
    {
      if (smartcontract.Profit == student.Smartcontract.Profit && smartcontract.Period == student.Smartcontract.Period && smartcontract.Percentage == student.Smartcontract.Percentage)
        Student_list += student;
      Number_of_students = Student_list.length;
    }

    def showlist(): String =
    {
      var info: String = "";
      for (i <- 0 until Student_list.length)
      {
        info += Student_list(i).Show();
      }
      return info;
    }

    def Evaluation(burse: Burse, platform: Platform): Unit =
    {
      if (balance < courseprice) {
        var toup: Tuple2[Double, Double] = Buy((courseprice - balance), burse, balance, fiat)
        balance = toup._1;
        fiat = toup._2;
      }

      var New_Platform_Tokens = platform.Get_Tokens + courseprice;
      platform.Set_Tokens(New_Platform_Tokens);
      balance = balance - courseprice;
      var bal = 0;
      for (i <- 0 until Smartcontract.Period) {
        for (i <- 0 until Student_list.length) {
          var grade = Random.nextInt(5);
          if (grade == 4) {
            if (burse.Get_Price > 1.5) {
              Student_list -= Student_list(i);
              Number_of_students = Student_list.length;
            }
            var price = courseprice * 1.1;
            give(platform, price);
            var New_Balance_1 = Student_list(i).Balance + price;
            Student_list(i).Set_Balance(New_Balance_1, burse, platform, courseprice);
            if (Student_list(i).Balance < courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Buy((courseprice - Student_list(i).Balance), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);

            }
            var New_balance_2 = Student_list(i).Balance - courseprice;

            Student_list(i).Set_Balance(New_balance_2, burse, platform, courseprice);
            if (Student_list(i).Balance > courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Sell((Student_list(i).Balance - courseprice), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            balance = balance + courseprice / 2;
            get(platform, courseprice);
            if (balance > courseprice) {
              var toup: Tuple2[Double, Double] = Sell((balance - courseprice), burse, balance, fiat)
              balance = toup._1;
              fiat = toup._2;
            }
            if (burse.Get_Price < 0.5) {
              platform.Buy(courseprice, burse, platform.Get_Tokens, platform.Get_Fiat);
            }

          }
          if (grade == 3) {
            if (burse.Get_Price > 1.5) {
              Student_list -= Student_list(i);
              Number_of_students = Student_list.length;
            }
            var price = courseprice * 1;
            give(platform, price);
            var New_Balance_1 = Student_list(i).Balance + price;
            Student_list(i).Set_Balance(New_Balance_1, burse, platform, courseprice);
            if (Student_list(i).Balance < courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Buy((courseprice - Student_list(i).Balance), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            var New_Balance_2 = Student_list(i).Balance - courseprice;
            Student_list(i).Set_Balance(New_Balance_2, burse, platform, courseprice);
            if (Student_list(i).Balance > courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Sell((Student_list(i).Balance - courseprice), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            balance = balance + courseprice / 2;
            get(platform, courseprice);
            if (balance > courseprice) {
              var toup: Tuple2[Double, Double] = Sell((balance - courseprice), burse, balance, fiat)
              balance = toup._1;
              fiat = toup._2;
            }
            if (burse.Get_Price < 0.5) {
              platform.Buy(courseprice, burse, platform.Get_Tokens, platform.Get_Fiat);
            }
          }
          if (grade == 2) {
            if (burse.Get_Price > 1.5) {
              Student_list -= Student_list(i);
              Number_of_students = Student_list.length;
            }
            var price = courseprice * 0.8;
            give(platform, price);
            var New_Balance_1 = Student_list(i).Balance + price - (courseprice - price);
            Student_list(i).Set_Balance(New_Balance_1, burse, platform, courseprice);
            if (Student_list(i).Balance < courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Buy((courseprice - Student_list(i).Balance), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            var New_Balance_2 = Student_list(i).Balance - courseprice;
            Student_list(i).Set_Balance(New_Balance_2, burse, platform, courseprice);
            if (Student_list(i).Balance > courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Sell((Student_list(i).Balance - courseprice), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            balance = balance + courseprice / 2;
            get(platform, courseprice);
            if (balance > courseprice) {
              var toup: Tuple2[Double, Double] = Sell((balance - courseprice), burse, balance, fiat)
              balance = toup._1;
              fiat = toup._2;
            }
            if (burse.Get_Price < 0.5) {
              platform.Buy(courseprice, burse, platform.Get_Tokens, platform.Get_Fiat);
            }

          }
          if (grade == 1) {
            if (burse.Get_Price > 1.5) {
              Student_list -= Student_list(i);
              Number_of_students = Student_list.length;
            }
            var price = courseprice * 0.7;
            give(platform, price);
            var New_Balance_1 = Student_list(i).Balance + price - (courseprice - price);
            Student_list(i).Set_Balance(New_Balance_1, burse, platform, courseprice);
            if (Student_list(i).Balance < courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Buy((courseprice - Student_list(i).Balance), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            var New_Balance_2 = Student_list(i).Balance - courseprice;
            Student_list(i).Set_Balance(New_Balance_2, burse, platform, courseprice);
            if (Student_list(i).Balance > courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Sell((Student_list(i).Balance - courseprice), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            balance = balance + courseprice / 2;
            get(platform, courseprice);
            if (balance > courseprice) {
              var toup: Tuple2[Double, Double] = Sell((balance - courseprice), burse, balance, fiat)
              balance = toup._1;
              fiat = toup._2;
            }
            if (burse.Get_Price < 0.5) {
              platform.Buy(courseprice, burse, platform.Get_Tokens, platform.Get_Fiat);
            }
          }
          if (grade == 0) {
            if (burse.Get_Price > 1.5) {
              Student_list -= Student_list(i);
              Number_of_students = Student_list.length;
            }
            var price = courseprice * 0.1;
            give(platform, price);
            var New_Balance_1 = Student_list(i).Balance + price - (courseprice - price);
            Student_list(i).Set_Balance(New_Balance_1, burse, platform, courseprice);
            if (Student_list(i).Balance < courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Buy((courseprice - Student_list(i).Balance), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }

            var New_Balance_2 = Student_list(i).Balance - courseprice;
            Student_list(i).Set_Balance(New_Balance_2, burse, platform, courseprice);
            if (Student_list(i).Balance > courseprice) {
              var toup: Tuple2[Double, Double] = Student_list(i).Sell((Student_list(i).Balance - courseprice), burse, Student_list(i).Balance, Student_list(i).Fiat)
              Student_list(i).Set_Balance(toup._1, burse, platform, courseprice);
              Student_list(i).Set_Fiat(toup._2);
            }
            balance = balance + courseprice / 2;
            get(platform, courseprice);
            if (balance > courseprice) {
              var toup: Tuple2[Double, Double] = Sell((balance - courseprice), burse, balance, fiat)
              balance = toup._1;
              fiat = toup._2;
            }
            if (burse.Get_Price < 0.5) {
              platform.Buy(courseprice, burse, platform.Get_Tokens, platform.Get_Fiat);
            }

          }
          println(grade);
        }

        def Get_Login: String = login;

        def Set_Login(Value: String) =
        {
          login = Value;
        }

        def Get_Password: String = password;

        def Set_Password(Value: String) =
        {
          password = Value;
        }

        def Get_Balance: Double = balance;

        def Set_Balance(Value: Double) =
        {
          balance = Value;
        }

        def Get_Fiat: Double = fiat;

        def Set_Fiat(Value: Double) =
        {
          fiat = Value;
        }

        def Get_Smartcontract: Smart_Contract = smartcontract;

        def Set_Smartcontract(Value: Smart_Contract) =
        {
          smartcontract = Value;
        }

        def Get_Courseprice: Int = courseprice;

        def Set_Courseprice(Value: Int) =
        {
          courseprice = Value;
        }

        def List_Student: ListBuffer[Student] = Student_list;
      }
    }
  }

}

object Main
{
  def main(args: Array[String]): Unit =
  {
    var Adress = new Adr("Ukraine","Kherson",9);
    var Smart__contract = new Smart_Contract(450,3,15);

    var teacher = new Teacher("Maxim","Poltorackiy",31,"poltorackiy@ukr.net",Adress,"poltorackiy12345", "1234567890",0,1000,Smart__contract,3);
    var student1 = new Student("Semen", "Bocharov", 19,"semen@gmail.com",Adress,"SEAmen","3228228322",0,1000,Smart__contract);
    var student2 = new Student("Nikolay", "Vasilkov", 17,"vasilkov@ukr.net",Adress,"vasilkov322","WaaWaaWaa",0,1000,Smart__contract);
    var student3 = new Student("Oleg", "Pchelkin", 18,"oleg@gmail.com",Adress,"Pchela","BzzzzzB",0,1000,Smart__contract);
    var student4 = new Student("Maria", "Kovalenko", 19,"Masha@gmail.com",Adress,"MashaKovalenko","333222000",0,1000,Smart__contract);
    var student5 = new Student("Kiril","Suprun", 18, "Kirill@ukr.net",Adress,"Kirieshka", "0987654321",0,1000,Smart__contract);
    var student6 = new Student("Anya","Voroncova",17,"Voroncova@ukr.net",Adress,"Anna","88005553555",0,1000,Smart__contract);
    var burse = new Burse();
    var _platform_ = new Platform();

    teacher.Add_in_List(student1);
    teacher.Add_in_List(student2);
    teacher.Add_in_List(student3);
    teacher.Add_in_List(student4);
    teacher.Add_in_List(student5);
    teacher.Add_in_List(student6);

    println(teacher.Show());
    println(teacher.showlist());
    teacher.Evaluation(burse, _platform_);
    println(teacher.Show());
    println(teacher.showlist());

  }
}