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


class MTBQueryRolesSPI extends RolesSPI
{
  override def getInstance: Roles =
    MTBQueryRoles
}

