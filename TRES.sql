CREATE OR REPLACE PACKAGE TRES as
   
    function informe(vEsmid number) return clob;
    
    procedure generarInforme(vEsmid number);
    
    procedure procesarDestinatario(vEsmid number,vNif varchar2);
    
    function selectDestinatarios(vEsmid number) return varchar2;
    
    procedure editarEscritoFirmado(vEsmid number);
    
end TRES;