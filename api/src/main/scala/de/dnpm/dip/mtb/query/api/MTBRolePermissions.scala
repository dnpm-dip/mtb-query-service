package de.dnpm.dip.mtb.query.api



import de.dnpm.dip.service.auth._
import de.dnpm.dip.service.query.QueryPermissions



object MTBPermissions extends QueryPermissions("MTB")


class MTBPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    MTBPermissions
}



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


class MTBRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBRoles
}

