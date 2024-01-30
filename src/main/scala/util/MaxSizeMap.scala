package util

class MaxSizeMap[K, V] private (map : Map[K,V], val maxSize: Int) extends  Map[K, V]: 

  override def size: Int = map.size

  override def iterator: Iterator[(K, V)] = map.iterator

  override def removed(key: K): Map[K, V] = new MaxSizeMap(map.removed(key), maxSize)

  override def updated[V1 >: V](key: K, value: V1): Map[K, V1] = 
    
    if map.size < maxSize then
      new MaxSizeMap(map.updated(key, value), maxSize)
    else
      val newMap = Map(key -> value)
      new MaxSizeMap(newMap, maxSize)

  override def get(key: K): Option[V] = map.get(key)

  override def apply(key: K): V = map(key)    


object MaxSizeMap:
  val defaultMaxSize = math.pow(2, 20).toInt - 1
  def empty[K,V](maxSize: Int = defaultMaxSize): MaxSizeMap[K, V] = new MaxSizeMap(Map.empty[K,V], maxSize)
  def apply[K,V](map : Map[K,V], maxSize: Int = defaultMaxSize): MaxSizeMap[K, V] = new MaxSizeMap[K, V](map, maxSize)