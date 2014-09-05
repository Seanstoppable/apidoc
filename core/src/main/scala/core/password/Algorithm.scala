package core.password

import org.mindrot.jbcrypt.BCrypt

case class HashedPassword(hash: String)

sealed trait Algorithm {

  /**
   * Uniquely identifies this password algorithm
   */
  def key: String

  /**
   * Hashes the provided String, returning the hashed value
   */
  def hash(password: String): HashedPassword

  /**
   * Check if a cleartext password is valid
   */
  def check(candidate: String, hashed: String): Boolean

}

case class BcryptAlgorithm(override val key: String) extends Algorithm {

  private val LogRounds = 10

  override def hash(password: String): HashedPassword = {
    val salt = BCrypt.gensalt(LogRounds)
    HashedPassword(BCrypt.hashpw(password, salt))
  }

  override def check(candidate: String, hashed: String): Boolean = {
    BCrypt.checkpw(candidate, hashed)
  }

}

/**
 * Used only when fetching values hashed with an unknown algorithm - never succeeds.
 */
case class UnknownAlgorithm(override val key: String) extends Algorithm {

  override def hash(password: String): HashedPassword = {
    sys.error("UnknownAlgorithm: Unsupported operation")
  }

  override def check(candidate: String, hashed: String) = false

}

object Algorithm {

  val All = Seq(
    new BcryptAlgorithm("bcrypt"),
    new UnknownAlgorithm("unknown")
  )

  val Latest = fromString("bcrypt").getOrElse {
    sys.error("Could not find latest algorithm")
  }

  val Unknown = fromString("unknown").getOrElse {
    sys.error("Could not find unknown algorithm")
  }

  def fromString(value: String): Option[Algorithm] = {
    All.find(_.key == value)
  }

}

