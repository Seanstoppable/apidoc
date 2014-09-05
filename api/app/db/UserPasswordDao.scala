package db

import core.password.Algorithm
import com.gilt.apidoc.models.User
import lib.Constants
import anorm._
import play.api.db._
import play.api.Play.current
import play.api.libs.json._
import java.util.UUID
import java.sql.Connection
import org.apache.commons.codec.binary.Base64

case class UserPassword(guid: UUID, userGuid: UUID, algorithm: Algorithm, hash: String)

object UserPasswordDao {

  private val BaseQuery = """
    select guid::varchar, user_guid::varchar, algorithm_key, hash
      from user_passwords
     where deleted_at is null
  """

  private val InsertQuery = """
    insert into user_passwords
    (guid, user_guid, algorithm_key, hash, created_by_guid, updated_by_guid)
    values
    ({guid}::uuid, {user_guid}::uuid, {algorithm_key}, {hash}, {created_by_guid}::uuid, {updated_by_guid}::uuid)
  """

  private val SoftDeleteByUserGuidQuery = """
    update user_passwords
       set deleted_by_guid = {deleted_by_guid}::uuid, deleted_at = now()
     where user_guid = {user_guid}::uuid
       and deleted_at is null
  """

  def create(user: User, userGuid: UUID, cleartextPassword: String) {
    DB.withTransaction { implicit c =>
      softDeleteByUserGuid(c, user, userGuid)
      doCreate(c, user.guid, userGuid, cleartextPassword)
    }
  }

  private[db] def doCreate(
    implicit c: java.sql.Connection,
    creatingUserGuid: UUID,
    userGuid: UUID,
    cleartextPassword: String
  ) {
    val guid = UUID.randomUUID
    val algorithm = Algorithm.Latest
    val hashedPassword = algorithm.hash(cleartextPassword)

    SQL(InsertQuery).on(
      'guid -> guid,
      'user_guid -> userGuid,
      'algorithm_key -> algorithm.key,
      'hash -> new String(Base64.encodeBase64(hashedPassword.hash.getBytes)),
      'created_by_guid -> creatingUserGuid,
      'updated_by_guid -> creatingUserGuid
    ).execute()
  }

  private[this] def softDeleteByUserGuid(implicit c: Connection, user: User, userGuid: UUID) {
    SQL(SoftDeleteByUserGuidQuery).on('deleted_by_guid -> user.guid, 'user_guid -> userGuid).execute()
  }

  def isValid(userGuid: UUID, cleartextPassword: String): Boolean = {
    findByUserGuid(userGuid) match {
      case None => false
      case Some(up: UserPassword) => {
        up.algorithm.check(cleartextPassword, up.hash)
      }
    }
  }

  private[db] def findByUserGuid(userGuid: UUID): Option[UserPassword] = {
    val sql = Seq(
      Some(BaseQuery.trim),
      Some("and user_passwords.user_guid = {guid}::uuid")
    ).flatten.mkString("\n   ")

    val bind = Seq[Option[NamedParameter]](
      Some('guid -> userGuid)
    ).flatten

    DB.withConnection { implicit c =>
      SQL(sql).on(bind: _*)().toList.map { row =>
        UserPassword(
          guid = UUID.fromString(row[String]("guid")),
          userGuid = UUID.fromString(row[String]("user_guid")),
          algorithm = Algorithm.fromString(row[String]("algorithm_key")).getOrElse(Algorithm.Unknown),
          hash = new String(Base64.decodeBase64(row[String]("hash").getBytes))
        )
      }.toSeq.headOption
    }
  }

}
