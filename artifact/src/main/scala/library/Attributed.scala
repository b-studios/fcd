package might

/*
 The contents of this file are taken (adapted) from Matt Might's
 implementation of parsing with derivatives. The original implementation can
 be found online at:

 http://matt.might.net/articles/parsing-with-derivatives/
 */

/** A collection of attributes which must be computed by iteration to a fixed
  * point.
  */
trait Attributed {
  private var generation = -1
  private var stabilized = false

  /** An attribute computable by fixed point.
    *
    * @param bottom
    *   the bottom of the attribute's lattice.
    * @param join
    *   the lub operation on the lattice.
    * @param wt
    *   the partial order on the lattice.
    */
  abstract class Attribute[A](
      bottom: A,
      join: (A, A) => A,
      wt: (A, A) => Boolean
  ) {
    private var currentValue: A = bottom
    private var compute: () => A = null
    private var fixed = false

    /** Sets the computation that updates this attribute.
      *
      * @param computation
      *   the computation that updates this attribute.
      */
    def :=(computation: => A) = { compute = (() => computation) }

    /** Permanently fixes the value of this attribute.
      *
      * @param value
      *   the value of this attribute.
      */
    def :==(value: A) = {
      currentValue = value
      fixed = true
    }

    /** Recomputes the value of this attribute.
      */
    def update(): Unit = {
      if fixed then return

      val old = currentValue
      val newValue = compute()

      if !wt(newValue, currentValue) then {
        currentValue = join(newValue, currentValue)
        FixedPoint.changed = true
      }
    }

    /** The current value of this attribute.
      */
    def value: A = {
      /*
       When the value of this attribute is requested, there are
       three possible cases:
       (1) It's already been computed (this.stabilized);
       (2) It's been manually set (this.fixed); or
       (3) It needs to be computed (generation < FixedPoint.generation).
       */
      if fixed || stabilized || generation == FixedPoint.generation then
        return currentValue

      fix()
      if FixedPoint.stabilized then stabilized = true
      currentValue
    }
  }

  // Subsumption tests for attributes:
  protected def implies(a: Boolean, b: Boolean) = !a || b
  protected def follows(a: Boolean, b: Boolean) = !b || a
  protected def updateAttributes(): Unit

  private def fix() = {
    this.generation = FixedPoint.generation

    if FixedPoint.master eq null then {
      FixedPoint.master = this

      FixedPoint.generation += 1
      FixedPoint.changed = false
      updateAttributes()
      while (FixedPoint.changed) {
        FixedPoint.generation += 1
        FixedPoint.changed = false
        updateAttributes()
      }

      FixedPoint.stabilized = true
      FixedPoint.generation += 1
      updateAttributes()
      FixedPoint.reset()
    } else updateAttributes()
  }
}

/** FixedPoint tracks the state of a fixed point algorithm for the attributes of
  * a grammar.
  *
  * In case there are fixed points running in multiple threads, each attribute
  * is thread-local.
  */
private object FixedPoint {
  private val _stabilized = ThreadLocal[Boolean]()
  _stabilized.set(false)
  def stabilized = _stabilized.get
  def stabilized_=(v: Boolean) = { _stabilized.set(v) }

  private val _running = ThreadLocal[Boolean]()
  _running.set(false)
  def running = _running.get
  def running_=(v: Boolean) = { _running.set(v) }

  private val _changed = ThreadLocal[Boolean]()
  _changed.set(false)
  def changed = _changed.get
  def changed_=(v: Boolean) = { _changed.set(v) }

  private val _generation = ThreadLocal[Int]()
  _generation.set(0)
  def generation = _generation.get
  def generation_=(v: Int) = { _generation.set(v) }

  private val _master = ThreadLocal[Object]()
  _master.set(null)
  def master = _master.get
  def master_=(v: Object) = { _master.set(v) }

  /** Resets all of the fixed point variables for this thread.
    */
  def reset() = {
    this.stabilized = false
    this.running = false
    this.master = null
    this.changed = false
    this.generation = 0
  }
}
