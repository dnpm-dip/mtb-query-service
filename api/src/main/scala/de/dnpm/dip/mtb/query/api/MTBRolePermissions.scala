package de.dnpm.dip.mtb.query.api



import de.dnpm.dip.service.auth.{
  Permission,
  Permissions,
  PermissionsSPI,
  Role,
  Roles,
  RolesSPI
}
import de.dnpm.dip.service.query.QueryPermissions


object MTBPermissions extends QueryPermissions("MTB")


/*
object MTBPermissions extends Enumeration with Permissions
{
  
  import scala.language.implicitConversions

  val SubmitQuery        = Value
  val ViewResultSummary  = Value
  val ViewPatientMatches = Value
  val ViewPatientRecord  = Value


  implicit def toPermission(v: Value): Permission =
    Permission(v.toString)


  override val permissions: Set[Permission] =
    this.values
      .map(_.toString)
      .toSet
      .map(Permission(_))

  def unapply(p: String): Option[Value] =
    values.find(_.toString == p)

}  

object MTBPermissions extends Permissions
{
  
  val SubmitQuery =
    Permission("mtb_query_submit")

  val ViewResultSummary =
    Permission("mtb_result_summary_read")

  val ViewPatientMatches =
    Permission("mtb_patient_matches_read")

  val ViewPatientRecord =
    Permission("mtb_patient_record_read")


  override val permissions: Set[Permission] =
    Set(
      SubmitQuery, 
      ViewResultSummary,
      ViewPatientMatches,
      ViewPatientRecord
    )

}  
*/

object MTBRoles extends Roles
{

  import MTBPermissions._


  val BasicMTBMember =
    Role(
      "BasicMTBMember",
      (permissions - ReadPatientRecord)
    )

  val PrivilegedMTBMember =
    Role(
      "PrivilegedMTBMember",
      permissions
    )

  override val roles: Set[Role] =
    Set(
      BasicMTBMember,
      PrivilegedMTBMember
    )

}


class MTBPermissionsSPI extends PermissionsSPI
{
  override def getInstance: Permissions =
    MTBPermissions
}


class MTBRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBRoles
}

