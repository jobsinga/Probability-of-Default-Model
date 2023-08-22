DELIMITER $$
# Stored procedure for building PD model
	
CREATE PROCEDURE sp_Retail_PD_Model_Implement(
        IN Reporting_Year VARCHAR(5),
        IN Reporting_Time VARCHAR(3)
)
BEGIN

    -- 'Y2018' 'Dec'
    set @Reporting_Date = last_day(str_to_date(concat(Reporting_Time, ' 1 ', right(Reporting_Year,4)), '%M %d %Y'));
    set @MoodyDate = date_sub(@Reporting_Date, interval 1 month);
    set @Moody_Year = concat('Y',YEAR(@MoodyDate));
    set @Moody_Time = Left(date_format(@MoodyDate, '%M'), 3);

    DROP TEMPORARY TABLE IF EXISTS i_temp_t_loan_retail;
	CREATE TEMPORARY TABLE i_temp_t_loan_retail(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);

    TRUNCATE TABLE i_temp_t_loan_retail;
    INSERT INTO i_temp_t_loan_retail
    select
		  Year
		, Time
		, Loan_Number
		, CIF_Number
		, SL_Date
		, Funded_Date
		, Partner_Participation
		, Underwriter_Code
		, Branch
		, RemainingPrincipal_excl_Partner
        , RemainingPrincipal_excl_Partner + IFNull(Partner_Participation,0)				as RemainingPrincipal_incl_Partner
		, Appraisal_Value
		, Term
        , TIMESTAMPDIFF(month, Funded_Date, SL_Date)        as Age_At_SL
		, Postal_Code
        , LEFT(Postal_Code, 3)                              as FSA
		, Province
	    , City
		, IFNULL(Arrears_Days, 0)                           as Arrears_Days
		, Arrears_Status
		, IF(Funding_GDS / 100 > 1, 1, Funding_GDS / 100)   as Funding_GDS_Ratio
        , IF(Funding_TDS / 100 > 1, 1, Funding_TDS / 100)   as Funding_TDS_Ratio
		, CASE  WHEN Underwriter_Code = 'PRM'		Then 'PRIME'
				WHEN Branch in ( '2000', '2001','2010', '2011')	THEN 'PRIME'
				WHEN Underwriter_Code = 'CMA'		Then 'Alt'
		  END as Alt_Prime_Indicator
		, CASE
		    WHEN (Arrears_Status like '%Tech%' OR Arrears_Status like '%not*arrear%')	THEN 0
			WHEN IFNULL(Arrears_Days, 0) <30											THEN 0
			WHEN IFNULL(Arrears_Days, 0) <60											THEN 1
			WHEN IFNULL(Arrears_Days, 0) >=60											THEN 2
		  END as Delinquency_Status
    from t_loan_retail
    where Year = Reporting_Year and Time = Reporting_Time;

/*====================================================================
					 Query 1.1 Bring regional information and beacon/BNI score
______________________________________________________________________
Description:

======================================================================*/
    DROP TEMPORARY TABLE IF EXISTS ii_temp_t_loan_retail;
	CREATE TEMPORARY TABLE ii_temp_t_loan_retail(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , Metro_Name                            VARCHAR(50)
        , Province_State_Name                   VARCHAR(25)
        , Max_Beacon_Score_App                  INT
        , Max_BNI_Score_App                     INT
	    , SL_Date_HPI                           INT
	    , Funded_Date_HPI                       INT
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);


    TRUNCATE TABLE ii_temp_t_loan_retail;
    INSERT INTO ii_temp_t_loan_retail
    select
        ttlr.*
        , tfm.Metro_Name
        , tPM.Province_State_Name
        , tbb.Max_Beacon_Score_App
        , tbb.Max_BNI_Score_App
        -- CAST(CONCAT(cast(Year(ttlr.SL_Date) as CHAR), cast(DATE_FORMAT(ttlr.SL_Date, '%m') as CHAR)) as SIGNED) as SL_Date_HPI
        , CAST(CONCAT(cast(DATE_FORMAT(ttlr.SL_Date, '%Y') as CHAR), cast(DATE_FORMAT(ttlr.SL_Date, '%m') as CHAR)) as SIGNED) as SL_Date_HPI
        -- IF(Year(ttlr.Funded_Date)<2005, '200501', CAST(CONCAT(cast(Year(ttlr.Funded_Date) as CHAR), cast(DATE_FORMAT(ttlr.Funded_Date, '%m') as CHAR)) as SIGNED)) as Funded_Date_HPI
        , IF(Year(ttlr.Funded_Date)<2005, '200501', CAST(CONCAT(cast(DATE_FORMAT(ttlr.Funded_Date, '%Y') as CHAR), cast(DATE_FORMAT(ttlr.Funded_Date, '%m') as CHAR)) as SIGNED)) as Funded_Date_HPI
    from i_temp_t_loan_retail                       as ttlr
    LEFT JOIN t_fsa_mapping                         as tfm
        on ttlr.FSA = tfm.FSA
    LEFT JOIN t_beacon_bni                          as tbb
        on ttlr.Loan_Number = tbb.Loan_Number
    LEFT JOIN t_Province_Mapping                    as tPM
        on ttlr.Province = tPM.Province;



/*====================================================================
					 Query 2 Proceed Macro Driver transformation
______________________________________________________________________
Description:

======================================================================*/
    DROP TEMPORARY TABLE IF EXISTS temp_t_Moody_Macro;
	CREATE TEMPORARY TABLE temp_t_Moody_Macro(
		Date									DATE
		, ur					                DECIMAL(25,10)
		, Date_1Q_Lag						    DATE
		, ur_1Q_Lag								DECIMAL(25,10)
	    , Diff_2Q                               DECIMAL(25,10)
	    , Comments                              VARCHAR(50));

    TRUNCATE TABLE temp_t_Moody_Macro;
    INSERT INTO temp_t_Moody_Macro
    WITH tmmh AS
    (select
        Date
        , Economic_Indicator
        , Geography_Code
        , Value
        , 'Moody_Historical'            as Comments
    from t_moody_macro_historical
    where Year = @Moody_Year and Time = @Moody_Time
    UNION
    select
        Date
        , Economic_Indicator
        , Geography_Code
        , Value
        , 'Moody_Forecast'              as Comments
    from t_moody_macro_forecast
    where Year = @Moody_Year and Time = @Moody_Time)
    select
        t1.Date                         as Date
        , t1.Value                      as ur
        , t2.Date                       as Date_1Q_Lag
        , t2.Value                      as ur_1Q_Lag
        , t1.Value / t2.Value - 1       as Diff_2Q
        , t1.Comments                   as Comments
    from tmmh                       as t1
    LEFT JOIN tmmh                  as t2
        ON t1.Date = LAST_DAY(DATE_ADD(t2.Date, INTERVAL 3 MONTH))
    ;

    #Alternative approach
#     WITH tmmh2 AS
#     (select
#         Date
#         , Economic_Indicator
#         , Geography_Code
#         , Value
#         , 'Moody_Historical'            as Comments
#     from t_moody_macro_historical
#     where Year = 'Y2018' and Time = 'Nov'
#     UNION
#     select
#         Date
#         , Economic_Indicator
#         , Geography_Code
#         , Value
#         , 'Moody_Forecast'              as Comments
#     from t_moody_macro_forecast
#     where Year = 'Y2018' and Time = 'Nov')
#     select
#         Date
#         , Value
#         , LAG(Date, 3) OVER (
#             ORDER BY Date
#         ) as Date_1Q_Lag
#         , LAG(Value, 3) OVER (
#             ORDER BY Date
#         ) as ur_1Q_Lag
#         , (Value - LAG(Value, 3) OVER (
#             ORDER BY Date)) / 100       as UR_First_Diff
#     from tmmh2
# ;

/*====================================================================
					 Query 3.0 Bring HPI Index
______________________________________________________________________
Description:

======================================================================*/
    DROP TEMPORARY TABLE IF EXISTS temp_t_HPI;
	CREATE TEMPORARY TABLE temp_t_HPI (
		  Loan_Number									INT
	    , SL_Date                                       DATE
        , SL_Date_HPI                                   INT
        , Funded_Date                                   DATE
        , Funded_Date_HPI                               INT
        , Province                                      VARCHAR(5)
        , Province_State_Name                           VARCHAR(25)
	    , HPI_Index_Prov_Base                           DECIMAL(20,10)
        , HPI_Index_Prov_Curr                           DECIMAL(20,10)
        , HPI_Index_Can_Base                            DECIMAL(20,10)
        , HPI_Index_Can_Curr                            DECIMAL(20,10)
    );

    TRUNCATE TABLE temp_t_HPI;
    INSERT INTO temp_t_HPI
    select
        Loan_Number
        , SL_Date
        , SL_Date_HPI
        , Funded_Date
        , Funded_Date_HPI
        , Province
        , Province_State_Name
        , thp_base.HPI_Index                as HPI_Index_Prov_Base
        , thp_curr.HPI_Index                as HPI_Index_Prov_Curr
        , thn_base.HPI_Index                as HPI_Index_Can_Base
        , thn_curr.HPI_Index                as HPI_Index_Can_Curr
    from ii_temp_t_loan_retail          as ittlr
    -- Provincial Level
    LEFT JOIN t_hpi_province            as thp_base
        ON ittlr.Province_State_Name = thp_base.HPI_Province
        and ittlr.Funded_Date_HPI = thp_base.date
    LEFT JOIN t_hpi_province            as thp_curr
        ON ittlr.Province_State_Name = thp_curr.HPI_Province
        and ittlr.SL_Date_HPI = thp_curr.date
    -- National Level
    Left Join t_hpi_national            as thn_base
        ON ittlr.Funded_Date_HPI = thn_base.date
    Left Join t_hpi_national            as thn_curr
        ON ittlr.SL_Date_HPI = thn_curr.date;


/*====================================================================
					 Query 3.1 Adjusted Appraisal Value by HPI
______________________________________________________________________
Description:

======================================================================*/
    DROP TEMPORARY TABLE IF EXISTS iii_temp_t_loan_retail;
	CREATE TEMPORARY TABLE iii_temp_t_loan_retail(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , Metro_Name                            VARCHAR(50)
        , Province_State_Name                   VARCHAR(25)
        , Max_Beacon_Score_App                  INT
        , Max_BNI_Score_App                     INT
	    , SL_Date_HPI                           INT
	    , Funded_Date_HPI                       INT
        , HPI_Index_Prov_Base                   DECIMAL(20,10)
        , HPI_Index_Prov_Curr                   DECIMAL(20,10)
        , HPI_Index_Can_Base                    DECIMAL(20,10)
        , HPI_Index_Can_Curr                    DECIMAL(20,10)
        , Appr_Val_Prov                         DECIMAL(25,10)
        , Appr_Val_Can                          DECIMAL(25,10)
	    , Appr_Val_WF                           DECIMAL(25,10)
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);

    TRUNCATE TABLE iii_temp_t_loan_retail;
    INSERT INTO iii_temp_t_loan_retail
    select
        ittlr2.*
        -- HPI
        , HPI_Index_Prov_Base
        , HPI_Index_Prov_Curr
        , HPI_Index_Can_Base
        , HPI_Index_Can_Curr
        -- Appraisal value adjusted by HPI
        , ittlr2.Appraisal_Value * (ttH.HPI_Index_Prov_Curr / ttH.HPI_Index_Prov_Base)  as Appr_Val_Prov
        , ittlr2.Appraisal_Value * (ttH.HPI_Index_Can_Curr / ttH.HPI_Index_Can_Base)    as Appr_Val_Can
        , IFNULL(ittlr2.Appraisal_Value * (ttH.HPI_Index_Prov_Curr / ttH.HPI_Index_Prov_Base), ittlr2.Appraisal_Value * (ttH.HPI_Index_Can_Curr / ttH.HPI_Index_Can_Base))
                                    AS Appr_Val_WF
    from ii_temp_t_loan_retail          as ittlr2
    LEFT JOIN temp_t_HPI                as ttH
        ON ittlr2.Loan_Number = ttH.Loan_Number
        and ittlr2.SL_Date = ttH.SL_Date;

    DROP TEMPORARY TABLE IF EXISTS iv_temp_t_loan_retail;
	CREATE TEMPORARY TABLE iv_temp_t_loan_retail(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , Metro_Name                            VARCHAR(50)
        , Province_State_Name                   VARCHAR(25)
        , Max_Beacon_Score_App                  INT
        , Max_BNI_Score_App                     INT
	    , SL_Date_HPI                           INT
	    , Funded_Date_HPI                       INT
        , HPI_Index_Prov_Base                   DECIMAL(20,10)
        , HPI_Index_Prov_Curr                   DECIMAL(20,10)
        , HPI_Index_Can_Base                    DECIMAL(20,10)
        , HPI_Index_Can_Curr                    DECIMAL(20,10)
        , Appr_Val_Prov                         DECIMAL(25,10)
        , Appr_Val_Can                          DECIMAL(25,10)
	    , Appr_Val_WF                           DECIMAL(25,10)
	    , LTV_Incl_Part_Prov_WF                 DECIMAL(25,10)
        , ur_2Q_Diff_2Q_Lag                     DECIMAL(25,10)
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);

    TRUNCATE TABLE iv_temp_t_loan_retail;
    INSERT INTO iv_temp_t_loan_retail
    select
        ittlr3.*
        , CASE
            WHEN Appr_Val_WF = 0                        THEN NULL
            ELSE RemainingPrincipal_incl_Partner / Appr_Val_WF
                                                        END AS LTV_Incl_Part_Prov_WF
        , ttMM.Diff_2Q                                  as ur_2Q_Diff_2Q_Lag
    from iii_temp_t_loan_retail                 as ittlr3
    LEFT JOIN temp_t_Moody_Macro                as ttMM
        ON ittlr3.SL_Date = ttMM.Date;


/*====================================================================
					 Query 4 Insert input table into Database
______________________________________________________________________
Description:

======================================================================*/
    delete from t_Retail_PD_Model_Input
    where Year = Reporting_Year and time = Reporting_Time;

    INSERT INTO t_Retail_PD_Model_Input
    select
          Year
        , Time
        , Loan_Number
        , CIF_Number
        , SL_Date
        , Funded_Date
        , Partner_Participation
        , Underwriter_Code
        , Branch
        , RemainingPrincipal_excl_Partner
        , RemainingPrincipal_incl_Partner
        , Appraisal_Value
        , Term
        , Age_At_SL
        , Postal_Code
        , FSA
        , Province
        , City
        , Arrears_Days
        , Arrears_Status
        , Funding_GDS_Ratio
        , Funding_TDS_Ratio
        , Alt_Prime_Indicator
        , Delinquency_Status
        , Metro_Name
        , Province_State_Name
        , Max_Beacon_Score_App
        , Max_BNI_Score_App
        , SL_Date_HPI
        , Funded_Date_HPI
        , HPI_Index_Prov_Base
        , HPI_Index_Prov_Curr
        , HPI_Index_Can_Base
        , HPI_Index_Can_Curr
        , Appr_Val_Prov
        , Appr_Val_Can
        , LTV_Incl_Part_Prov_WF
        , ur_2Q_Diff_2Q_Lag
        , NOW()                                         as Last_Update_Date
        , USER()                                        as Last_Update_By
    from iv_temp_t_loan_retail;


/*====================================================================
					 Query 5 Bring WOE Mapping and fit driver into WOE bucket
______________________________________________________________________
Description:

======================================================================*/
    DROP TEMPORARY TABLE IF EXISTS i_temp_Loan_Retail_WOE;
	CREATE TEMPORARY TABLE i_temp_Loan_Retail_WOE(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , Metro_Name                            VARCHAR(50)
        , Province_State_Name                   VARCHAR(25)
        , Max_Beacon_Score_App                  INT
        , Max_BNI_Score_App                     INT
	    , SL_Date_HPI                           INT
	    , Funded_Date_HPI                       INT
        , HPI_Index_Prov_Base                   DECIMAL(20,10)
        , HPI_Index_Prov_Curr                   DECIMAL(20,10)
        , HPI_Index_Can_Base                    DECIMAL(20,10)
        , HPI_Index_Can_Curr                    DECIMAL(20,10)
        , Appr_Val_Prov                         DECIMAL(25,10)
        , Appr_Val_Can                          DECIMAL(25,10)
	    , Appr_Val_WF                           DECIMAL(25,10)
	    , LTV_Incl_Part_Prov_WF                 DECIMAL(25,10)
	    , ur_2Q_Diff_2Q_Lag                     DECIMAL(25,10)
        , PD_Region_WOE                         DECIMAL(25,10)
        , PD_Delq_WOE                           DECIMAL(25,10)
        , PD_TDS_GDS_WOE                        DECIMAL(25,10)
        , PD_Term_MOB_WOE                       DECIMAL(25,10)
        , PD_LTV_WOE                            DECIMAL(25,10)
        , PD_Beacon_BNI_WOE                     DECIMAL(25,10)
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);

    TRUNCATE TABLE i_temp_Loan_Retail_WOE;
    INSERT INTO i_temp_Loan_Retail_WOE
	select
		a.*
		, COALESCE(b.WOE, c.WOE)							as PD_Region_WOE
		, d.WOE             								as PD_Delq_WOE
		, ifnull(e.WOE, 0)							        as PD_TDS_GDS_WOE
		, g.WOE												as PD_Term_MOB_WOE
		, COALESCE(h.WOE, i.WOE)							as PD_LTV_WOE
		, COALESCE(j.WOE, k.WOE, l.WOE, m.WOE)				as PD_Beacon_BNI_WOE
	from iv_temp_t_loan_retail as a
	-- Region
	left join t_woe_Region as b
		on a.Metro_Name = b.Metro_Region
		and a.Province = b.Province
		and b.Valid_To_Date = '9999-12-31'
	left join t_woe_Region as c
		on c.Metro_Region = 'Other'
		and a.Province = c.Province
		and c.Valid_To_Date = '9999-12-31'
	-- Delq
	left join t_woe_Delq as d
		on a.Delinquency_Status = d.DelQ_Current
		and d.Valid_To_Date = '9999-12-31'
	-- GDS TDS
	left join t_woe_tds_gds as e
		on a.Funding_TDS_Ratio > e.TDS_Min
		and a.Funding_TDS_Ratio <= e.TDS_Max
		and a.Funding_GDS_Ratio > e.GDS_Min
		and a.Funding_GDS_Ratio <= e.GDS_Max
		and e.Valid_To_Date = '9999-12-31'
	-- Term
	left join t_woe_term_age as g
		on a.Term > g.Term_Min
		and a.Term <= g.Term_Max
		and a.Age_At_SL > g.MoB_Min
		and a.Age_At_SL <= g.MoB_Max
		and g.Valid_To_Date = '9999-12-31'
	-- LTV
	LEFT JOIN t_woe_ltv as h
		on  a.LTV_Incl_Part_Prov_WF > h.BF_LTV_Min
		and a.LTV_Incl_Part_Prov_WF <= h.BF_LTV_Max
		and a.Alt_Prime_Indicator = h.Alt_Prime_Ind
		and h.Valid_To_Date = '9999-12-31'
	LEFT JOIN t_woe_ltv as i
		on i.BF_LTV_Min is Null
		and a.Alt_Prime_Indicator = i.Alt_Prime_Ind
		and i.Valid_To_Date = '9999-12-31'
	-- Beacon
	LEFT JOIN t_woe_Beacon_BNI as j
		on a.Max_Beacon_Score_App > j.Beacon_Min
		and a.Max_Beacon_Score_App <= j.Beacon_Max
		and a.Max_BNI_Score_App > j.BNI_Min
		and a.Max_BNI_Score_App <= j.BNI_Max
		and j.Valid_To_Date = '9999-12-31'
	LEFT JOIN t_woe_Beacon_BNI as k
		on  IFNULL(k.Beacon_Min,-1) = IFNULL(a.Max_Beacon_Score_App,-1)
		and a.Max_BNI_Score_App > k.BNI_Min
		and a.Max_BNI_Score_App <= k.BNI_Max
		and k.Valid_To_Date = '9999-12-31'
	LEFT JOIN t_woe_Beacon_BNI as l
		on  IFNULL(l.BNI_Min,-1) = IFNULL(a.Max_BNI_Score_App,-1)
		and a.Max_Beacon_Score_App > l.Beacon_Min
		and a.Max_Beacon_Score_App <= l.Beacon_Max
		and l.Valid_To_Date = '9999-12-31'
	LEFT JOIN t_woe_Beacon_BNI as m
		on IFNULL(a.Max_Beacon_Score_App,-1) = IFNULL(m.Beacon_Min,-1)
		and IFNULL(a.Max_BNI_Score_App,-1) =  IFNULL(m.BNI_Min,-1)
		and m.Valid_To_Date = '9999-12-31';

/*====================================================================
					 Query 6 Bring coefficient to calculate PD
______________________________________________________________________
Description:

======================================================================*/
	SET @PD_Intercept           = (Select Intercept From t_woe_coeff);
	SET @PD_LTV                 = (Select LTV From t_woe_coeff);
	SET @PD_Beacon_BNI          = (Select Beacon_BNI From t_woe_coeff);
	SET @PD_Delq                = (Select Delq From t_woe_coeff);
	SET @PD_Term_Age            = (Select Term_Age From t_woe_coeff);
	SET @PD_Provincial_Risk     = (Select Provincial_Risk From t_woe_coeff);
	SET @PD_TDS_GDS             = (Select TDS_GDS From t_woe_coeff);
	SET @PD_UE                  = (Select UE From t_woe_coeff);

    DROP TEMPORARY TABLE IF EXISTS i_temp_Loan_Retail_WOE_PD;
	CREATE TEMPORARY TABLE i_temp_Loan_Retail_WOE_PD(
		  Year									VARCHAR(5)
		, Time									VARCHAR(3)
		, Loan_Number							INT
		, CIF_Number							INT
		, SL_Date								DATE
		, Funded_Date							DATE
		, Partner_Participation					DECIMAL(20,5)
		, Underwriter_Code						VARCHAR(5)
		, Branch								VARCHAR(25)
		, RemainingPrincipal_excl_Partner		DECIMAL(20,5)
	    , RemainingPrincipal_incl_Partner       DECIMAL(20,5)
		, Appraisal_Value						DECIMAL(20,5)
		, Term									INT
	    , Age_At_SL                             INT
		, Postal_Code							VARCHAR(25)
	    , FSA                                   VARCHAR(3)
		, Province								VARCHAR(5)
	    , City                                  VARCHAR(25)
		, Arrears_Days							INT
		, Arrears_Status						VARCHAR(100)
		, Funding_GDS_Ratio						DECIMAL(10,5)
		, Funding_TDS_Ratio						DECIMAL(10,5)
	    , Alt_Prime_Indicator                   VARCHAR(10)
	    , Delinquency_Status                    TINYINT
	    , Metro_Name                            VARCHAR(50)
        , Province_State_Name                   VARCHAR(25)
        , Max_Beacon_Score_App                  INT
        , Max_BNI_Score_App                     INT
	    , SL_Date_HPI                           INT
	    , Funded_Date_HPI                       INT
        , HPI_Index_Prov_Base                   DECIMAL(20,10)
        , HPI_Index_Prov_Curr                   DECIMAL(20,10)
        , HPI_Index_Can_Base                    DECIMAL(20,10)
        , HPI_Index_Can_Curr                    DECIMAL(20,10)
        , Appr_Val_Prov                         DECIMAL(25,10)
        , Appr_Val_Can                          DECIMAL(25,10)
	    , Appr_Val_WF                           DECIMAL(25,10)
	    , LTV_Incl_Part_Prov_WF                 DECIMAL(25,10)
	    , ur_2Q_Diff_2Q_Lag                     DECIMAL(25,10)
        , PD_Region_WOE                         DECIMAL(25,10)
        , PD_Delq_WOE                           DECIMAL(25,10)
        , PD_TDS_GDS_WOE                        DECIMAL(25,10)
        , PD_Term_MOB_WOE                       DECIMAL(25,10)
        , PD_LTV_WOE                            DECIMAL(25,10)
        , PD_Beacon_BNI_WOE                     DECIMAL(25,10)
	    , Log_Odds                              DECIMAL(25,10)
	    , PD_Final                              DECIMAL(25,10)
	    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
		);

    TRUNCATE TABLE i_temp_Loan_Retail_WOE_PD;
    INSERT INTO i_temp_Loan_Retail_WOE_PD
	select
	    *
        , @PD_Intercept
              + (@PD_LTV * PD_LTV_WOE)
              + (@PD_Beacon_BNI * PD_Beacon_BNI_WOE)
              + (@PD_Delq * PD_Delq_WOE)
	          + (@PD_Term_Age * PD_Term_MOB_WOE)
              + (@PD_TDS_GDS * PD_TDS_GDS_WOE)
              + ( @PD_Provincial_Risk * PD_Region_WOE)
              + ( @PD_UE * ur_2Q_Diff_2Q_Lag )
	                                                                as Log_Odds
	    , (1 / (1 + exp (- (@PD_Intercept
	                            + (@PD_LTV * PD_LTV_WOE)
	                            + (@PD_Beacon_BNI * PD_Beacon_BNI_WOE)
	                            + (@PD_Delq * PD_Delq_WOE)
	                            + (@PD_Term_Age * PD_Term_MOB_WOE)
	                            + (@PD_TDS_GDS * PD_TDS_GDS_WOE)
	                            + ( @PD_Provincial_Risk * PD_Region_WOE)
	                            + ( @PD_UE * ur_2Q_Diff_2Q_Lag )))))
	                                                                as PD_Final
	from i_temp_Loan_Retail_WOE;


    delete from t_Retail_PD_Model_Final
    where Year = Reporting_Year and time = Reporting_Time;

    INSERT INTO t_Retail_PD_Model_Final
    select
        Year
        , Time
        , SL_Date
        , Loan_Number
        , Alt_Prime_Indicator
        , PD_Region_WOE
        , PD_Delq_WOE
        , PD_TDS_GDS_WOE
        , PD_Term_MOB_WOE
        , PD_LTV_WOE
        , PD_Beacon_BNI_WOE
        , Log_Odds
        , PD_Final
        , NOW()                                         as Last_Update_Date
        , USER()                                        as Last_Update_By
    from i_temp_Loan_Retail_WOE_PD;



end$$

delimiter ;

   select *
        from t_Retail_PD_Model_Final
        where PD_Final>=1 or PD_Final<0 ;



    DROP TABLE IF EXISTS t_Retail_PD_Model_Final;
    CREATE TABLE t_Retail_PD_Model_Final
    (
        Year                varchar(5),
        Time                varchar(3),
        SL_Date             date,
        Loan_Number         int,
        Alt_Prime_Indicator varchar(10),
        PD_Region_WOE       decimal(25, 15),
        PD_Delq_WOE         decimal(25, 15),
        PD_TDS_GDS_WOE      decimal(25, 15),
        PD_Term_MOB_WOE     decimal(25, 15),
        PD_LTV_WOE          decimal(25, 15),
        PD_Beacon_BNI_WOE   decimal(25, 15),
        Log_Odds            decimal(25, 15),
        PD_Final            decimal(25, 15),
        Last_Update_Date    date,
        Last_Update_By      varchar(255)
    , PRIMARY KEY (Year, Time, SL_Date, Loan_Number)
    );


call sp_Retail_PD_Model_Implement('Y2018','Dec');


    CREATE VIEW vw_retail_pd_model_result_v2 as
    select
		  a.Year
		, a.Time
        , a.SL_Date
		, a.Loan_Number
		, a.Funded_Date
		, a.Term
	    , a.Age_At_SL
		, a.Province
	    , a.Metro_Name
		, a.Funding_GDS_Ratio
		, a.Funding_TDS_Ratio
	    , a.Alt_Prime_Indicator
	    , a.Delinquency_Status
        , a.Max_Beacon_Score_App
        , a.Max_BNI_Score_App
	    , a.LTV_Incl_Part_Prov_WF
	    , a.ur_2Q_Diff_2Q_Lag
        , b.PD_Region_WOE
        , b.PD_Delq_WOE
        , b.PD_TDS_GDS_WOE
        , b.PD_Term_MOB_WOE
        , b.PD_LTV_WOE
        , b.PD_Beacon_BNI_WOE
	    , b.Log_Odds
	    , b.PD_Final
	    , b.Last_Update_Date
	    , b.Last_Update_By
    from t_retail_pd_model_input        as a
    Left Join t_retail_pd_model_final   as b
        on a.Year = b.Year
        and a.Time = b.Time
        and a.Loan_Number = b.Loan_Number;



