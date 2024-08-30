package de.dnpm.dip.mtb.query.api


import de.dnpm.dip.service.query.UseCaseConfig
import de.dnpm.dip.mtb.model.MTBPatientRecord


sealed trait MTBConfig extends UseCaseConfig
{

  type PatientRecord = MTBPatientRecord

  type Criteria = MTBQueryCriteria

//  type Filter = MTBFilters

  type Results = MTBResultSet

}
