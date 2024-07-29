object InventorySystem {
  
  val inventory1: Map[Int, (String, Int, Double)] = Map(
    101 -> ("Widget", 10, 19.99),
    102 -> ("Gadget", 5, 29.99),
    103 -> ("Doodad", 15, 9.99)
  )
  
  val inventory2: Map[Int, (String, Int, Double)] = Map(
    102 -> ("Gadget", 7, 34.99),  
    104 -> ("Bin", 3, 49.99)
  )

  
  def getAllProductNames(inventory: Map[Int, (String, Int, Double)]): List[String] = {
    inventory.values.map(_._1).toList
  }

  
  def calculateTotalValue(inventory: Map[Int, (String, Int, Double)]): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  
  def isInventoryEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
    inventory.isEmpty
  }

  
  def mergeInventories(inventory1: Map[Int, (String, Int, Double)], inventory2: Map[Int, (String, Int, Double)]): Map[Int, (String, Int, Double)] = {
    val merged = (inventory1 ++ inventory2).map { case (id, (name, quantity, price)) =>
      (id, inventory1.get(id) match {
        case Some((_, q1, p1)) if price > p1 => (name, quantity + q1, price)
        case Some((_, q1, p1)) => (name, quantity + q1, p1)
        case None => (name, quantity, price)
      })
    }
    merged
  }

  
  def printProductDetails(inventory: Map[Int, (String, Int, Double)], productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) => println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
      case None => println(s"Product ID: $productId not found.")
    }
  }

  def main(args: Array[String]): Unit = {
   
    println(s"Product names in inventory1: ${getAllProductNames(inventory1)}")
    println(s"Total value of inventory1: ${calculateTotalValue(inventory1)}")
    println(s"Is inventory1 empty? ${isInventoryEmpty(inventory1)}")

    val mergedInventory = mergeInventories(inventory1, inventory2)
    println(s"Merged Inventory: $mergedInventory")

    printProductDetails(inventory1, 102)
    printProductDetails(inventory1, 999)  
  }
}
