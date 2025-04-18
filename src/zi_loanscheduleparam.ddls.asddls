@EndUserText.label: 'CDS for Loan Schedule Params'
define abstract entity ZI_LOANSCHEDULEPARAM
{
     @Consumption.valueHelpDefinition: [{ entity: { name: 'I_CompanyCodeSTDVH', element: 'CompanyCode' } }]
     @EndUserText.label: 'Company Code'
    comp_code : bukrs;
    @EndUserText.label: 'Date'
    month_year : abap.dats;
    
}
