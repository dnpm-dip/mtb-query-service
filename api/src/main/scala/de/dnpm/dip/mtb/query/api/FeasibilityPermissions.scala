package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.service.auth.{
  Permissions,
  PermissionEnumeration,
  PermissionsSPI,
  Role,
  Roles,
  RolesSPI
}


object FeasibilityPermissions extends PermissionEnumeration
{

  val SubmitQuery = Value(s"mtb_feasibility_query_submit")
  val ReadResults = Value(s"mtb_feasibility_results_read")


  override val display =
    Map(
      SubmitQuery -> "Machbarkeitsanalyse-Anfragen absetzen",
      ReadResults -> "Machbarkeitsanalyse-Ergebnisse einsehen",
    )


  override val description =
    Map(
      SubmitQuery -> "Machbarkeitsanalyse-Modul: Anfragen absetzen (lokal/föderiert)",
      ReadResults -> "Machbarkeitsanalyse-Modul: Ergebnisse abrufen/einsehen"
    )

}


class FeasibilityPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    FeasibilityPermissions
}


object FeasibilityRoles extends Roles
{

  val QueryRights =
    Role(
      "feasibility_query_rights",
      "Machbarkeitsanalyse-Rechte",
      FeasibilityPermissions.permissions,
      Some("Zugriffsrechte auf Machbarkeitsanalyse-Modul")
    )


  override val roles: Set[Role] =
    Set(
      QueryRights
    )

}

class FeasibilityRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    FeasibilityRoles
}


