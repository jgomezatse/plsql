CREATE OR REPLACE PACKAGE TRNT as
   
    function crmRes(vCrmid number) return varchar2;
    
end trnt;