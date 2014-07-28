package controllers

import apidoc.models.{ Metadata, Organization, User }
import models._
import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import scala.concurrent.Future

object Metadata extends Controller {

  implicit val context = scala.concurrent.ExecutionContext.Implicits.global

  def index(orgKey: String) = AuthenticatedOrg { implicit request =>
    val tpl = request.mainTemplate("Metadata")
    Ok(views.html.metadata.index(tpl.copy(settings = Some(SettingsMenu(section = Some(SettingSection.Metadata))))))
  }

  def edit(orgKey: String) = AuthenticatedOrg { implicit request =>
    val tpl = request.mainTemplate("Add Metadata")
    Ok(views.html.metadata.form(tpl, metadataForm))
  }

  def postEdit(orgKey: String) = AuthenticatedOrg.async { implicit request =>
    val tpl = request.mainTemplate("Add Metadata")
    val boundForm = metadataForm.bindFromRequest
    boundForm.fold (

      errors => Future {
        Ok(views.html.metadata.form(tpl, errors))
      },

      valid => {
        request.api.Metadata.post(
          orgKey = request.org.key,
          metadata = Metadata(valid.packageName)
        ).map { d =>
          Redirect(routes.Metadata.index(request.org.key)).flashing("success" -> s"Metadata added")
        }.recover {
          case response: apidoc.error.ErrorsResponse => {
            Ok(views.html.metadata.form(tpl, boundForm, response.errors.map(_.message)))
          }
        }
      }

    )

  }

  case class MetadataData(packageName: Option[String])
  private val metadataForm = Form(
    mapping(
      "package_name" -> text
    )(MetadataData.apply)(MetadataData.unapply)
  )
}
