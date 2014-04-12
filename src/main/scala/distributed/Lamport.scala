/**
 * Various notes and algorithms related to the following paper by
 * Leslie Lamport:
 *     Time, Clocks, and the Ordering of Events in a Distributed
 *     Systems
 *
 * @see http://www.stanford.edu/class/cs240/readings/lamport.pdf
 */

package distributed
object Lamport {
  /* Our system is a collection of processes. */
  trait System {
    val processes: List[Process]
    /* A process is a sequence of Events. */
    trait Process extends Clock[Event] {
      def events: Stream[Event]
    }
    /* What is an event varies from system to system. For one, it could
     * be the execution of a subprogram, for another it could be the
     * mere execution of an instruction.
     *
     * Inside a process, we assume a total ordering of events.
     *
     * Sending and receiving a message is assumed to be events in our
     * system.
     */
    trait Event {
      def isSendingOf(e: Event): Boolean
      def source: Process
    }

    /* '''Definition''': "->" relation
     * the "->" relation is the smallest relation satisfying the
     * following conditions:
     *   1. a and b events in the same process, if a comes before b
     *      then a -> b.
     *   2. if a is the sending of a message by one process and b the
     *      receiving of the same message by another process then
     *      a -> b
     *   3. if a -> b and b -> c then a -> c.
     *
     * 2 distintct events are said to be concurent if a -/-> b and
     * b -/-> a.
     */

    /* We note C,,i,, the clock of process P,,i,,.
     * '''Clock Condition''':
     * For any event a, b: if a -> b then C(a) < C(b)
     *
     * Then from our relation "->" we have:
     */
    object EventOrdering extends Ordering[Event] {
      def compare(a: Event, b: Event): Int = (a, b) match {
        case (x, y) if x.source == y.source =>  // C1
          x.source.timestamp(x) - x.source.timestamp(y)
        case (x, y) if x.isSendingOf(y) => -1   // C2
        case (x, y) if y.isSendingOf(x) => 1    // C2
        case _ => 0 // events are concurrent (not simultaneous, simply
                    // no ordering)
      }
    }

    /* Clock Implementation Rules:
     * To meet C1
     * IR1. Each process P,,i,, increments C,,i,, between any 2
     *      successive events.
     *
     * To meet C2
     * IR2. (a) if event a is the sending of a message ''m'' by
     *      process P,,i,,, then message ''m'' contains a timestamp
     *      T,,m,, = C,,i,,(a). (b) Upon receiving a message m, process
     *      P,,j,, sets C,,j,, greater than or equal to its present value
     *      and greater than T,,m,,.
     */
    trait Clock[T] {
      def timestamp: T => Int
      def clock: Int
    }
  }
}
