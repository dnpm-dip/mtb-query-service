package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.service.auth._
import de.dnpm.dip.service.query.{
  QueryPermissions,
  QueryRoles
}


object MTBQueryPermissions extends QueryPermissions("MTB")

class MTBQueryPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions = MTBQueryPermissions
}


object MTBQueryRoles extends QueryRoles(MTBQueryPermissions)

class MTBQueryRolesSPI extends RolesSPI
{
  override def getInstance: Roles = MTBQueryRoles
}


object MTBReportingPermissions extends PermissionEnumeration
{

  val SubmitReportRequest = Value("mtb_report_request") 
  val ReadReport          = Value("mtb_report_read") 

  override val display =
    Map(
      SubmitReportRequest -> "MTB-Reporting-Anfragen absetzen",
      ReadReport          -> "MTB-Reporting-Ergebnisse einsehen"
    )


  override val description =
    Map(
      SubmitReportRequest -> "MTB-Reporting-Modul: Anfrage zur Erzeugung von Reports absetzen",
      ReadReport          -> "MTB-Reporting-Modul: Report-Daten einsehen"
    )

}

object MTBReportingRoles extends Roles
{
  val ReportingRights = Role(
    name = "mtb_reporting_rights",
    display = "MTB-Reporting-Rechte",
    permissions = MTBReportingPermissions.permissions,
    Some("Berechtigungen zur Verwendung des MTB-Reporting-Funktionen"),
  )

  override val roles = Set(ReportingRights)
}


//TODO: Add to META-INF/services to make discoverable by ServiceLoader
class MTBReportingPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions = MTBReportingPermissions
}

class MTBReportingRolesSPI extends RolesSPI
{
  override def getInstance: Roles = MTBReportingRoles
}
