@Metadata.allowExtensions: true
@EndUserText.label: 'Loan Schedule'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity ZC_LOANSCHEDULE
  provider contract transactional_query
  as projection on ZR_LOANSCHEDULE
{
  key CompCode,
  key LoanNo,
  key MonthYear,
  EmployeeId,
  EmployeeName,
  LoanType,
  InstallmentAmount,
  ApprovedAmount,
  Approved,
  ApprovedBy,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChangedAt
  
}
