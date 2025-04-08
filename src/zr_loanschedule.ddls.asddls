@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Loan Schedule'
define root view entity ZR_LOANSCHEDULE
  as select from zloanschedule
{
  key comp_code as CompCode,
  key loan_no as LoanNo,
  key month_year as MonthYear,
  employee_id as EmployeeId,
  employee_name as EmployeeName,
  loan_type as LoanType,
  installment_amount as InstallmentAmount,
  approved_amount as ApprovedAmount,
  approved as Approved,
  approved_by as ApprovedBy,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  local_last_changed_at as LocalLastChangedAt
  
}
