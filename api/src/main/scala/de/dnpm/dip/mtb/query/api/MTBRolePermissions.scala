package de.dnpm.dip.mtb.query.api



import de.dnpm.dip.service.auth._
import de.dnpm.dip.service.query.{
  QueryPermissions,
  QueryRoles
}



object MTBQueryPermissions extends QueryPermissions("MTB")


class MTBQueryPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    MTBQueryPermissions
}


object MTBQueryRoles extends QueryRoles(MTBQueryPermissions)

/*
object MTBRoles extends Roles
{

  import MTBPermissions._


  val BasicMTBMember =
    Role(
      "MTBMemberBasic",
      (permissions - ReadPatientRecord),
      Some("MTB: Basis-Such-Rechte (nur Ergebnis-Zusammenfassungen)")
    )

  val PrivilegedMTBMember =
    Role(
      "MTBMemberPrivileged",
      permissions,
      Some("MTB: Privilegierte Such-Rechte (Ergebnis-Zusammenfassungen + Einsicht in Patienten-Akten)")
    )

  override val roles: Set[Role] =
    Set(
      BasicMTBMember,
      PrivilegedMTBMember
    )

}
*/

class MTBQueryRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBQueryRoles
}

