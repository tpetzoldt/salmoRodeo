include 'forc.f95'
module functions
use forcings
implicit none

contains

!! changes compared to normal quitzdorf version
!! - f_apsf function is smoother

! N consumption by phytoplankton (includes respiration offset)
function f_nkons_x_i (photx_i, rx_i, n, p, x_i, YNX, OPTNP, NFIXi) result (res)
    double precision, intent(in):: photx_i, rx_i, n, p, x_i, YNX, OPTNP, NFIXi
    double precision:: res
    
    IF ((n/p) < OPTNP) THEN ! this is the sum component in 1.2
        IF (NFIXi < 1d-4) THEN
            res = (photx_i - rx_i) / YNX * x_i
        ELSE
            res = 0d0 ! this is c and java version
        END IF
    ELSE
        res = (photx_i - rx_i) / YNX * x_i
    END IF
end function

! N release from sediment
function f_nsf (time, temp, vol, ased, n, NDSSTART, NDSEND, NDSMAX, KNDS, KNDST, ANSFMIN, KANSF) result (res)
    double precision, intent(in):: time, temp, vol, ased, n, NDSSTART, NDSEND, NDSMAX, KNDS, KNDST, ANSFMIN, KANSF
    double precision:: ansfs, ansfq, res 

    IF (NDSSTART <= mod(time, 36525d-2) .AND. mod(time, 36525d-2) < NDSEND) THEN ! 1.8
        ansfs = NDSMAX * n / (KNDS + n) * KNDST ** (temp - 4) ! release
    ELSE
        ansfs = 0d0 ! uptake
    END IF

    IF (NDSSTART <= mod(time, 36525d-2) .AND. mod(time, 36525d-2) < NDSEND) THEN
        ansfq = ANSFMIN ! release
    ELSE
        ansfq = ANSFMIN + KANSF * temp ! uptake
    END IF

    res = (ansfq - ansfs) / vol * ased ! 1.7
end function

! N denitrification of sedimenting phytoplankton (currently has no effect due to constant O2 saturation)
function f_nden (sum_xsed_i, g, temp, vol, depth, ased, o, n, d, KDEN, KNDS, LINDEN, KMINER, VD, SEZMAX, KSEZA, YOX, &
    RZOPT, RZMIN, GMAX, RZTMIN, TOPTZ, KO) result (res)
   double precision, intent(in):: sum_xsed_i, g, temp, vol, depth, ased, o, n, d, KDEN, KNDS, LINDEN, KMINER, VD, SEZMAX, & 
   KSEZA, YOX, RZOPT, RZMIN, GMAX, RZTMIN, TOPTZ, KO
   double precision:: lo_result, res

   IF (o <= LINDEN .AND. n > 0) THEN
       lo_result = f_lo(sum_xsed_i, g, temp, vol, depth, ased, o, d, KMINER, VD, SEZMAX, KSEZA, YOX, RZOPT, RZMIN, GMAX, &
        RZTMIN, TOPTZ, KO) ! 16.6

       res = n * KDEN * lo_result / (KNDS + n) ! 3.8
   ELSE
       res = 0d0
   END IF
end function

! P release from sediment
function f_apsf (temp, o, n, npsfmode, APSFMAX, APSFMIN, KAPSF, LINDEN, APSFT) result (res)
    double precision, intent(in):: temp, o, n, npsfmode, APSFMAX, APSFMIN, KAPSF, LINDEN, APSFT
    double precision:: b1, b2, apsf_result, res

    IF (npsfmode == 1) THEN ! 4.10 -> change from previous versions
        b1 = n - 0.3 * LINDEN
    
        IF (b1 <= 0) THEN
            apsf_result = APSFMAX
        ELSE
            apsf_result = (APSFMAX - APSFMIN) * ((1/b1)/((1/(KAPSF - 0.3 * LINDEN)) + (1/b1))) + APSFMIN
        END IF
    ELSE
        b2 = o + n / 0.3 - LINDEN
    
        IF (b2 <= 0) THEN
            apsf_result = APSFMAX
        ELSE
            apsf_result = (APSFMAX - APSFMIN) * ((1/b2)/((1/(KAPSF - LINDEN)) + (1/b2))) + APSFMIN
        END IF
    END IF

    res = apsf_result * APSFT ** (temp - 4) ! temp correction
end function

! photosynthesis of phytoplankton group i
function f_photx_i (light_extinction_sum, temp, depth, ired, n, p, x_i, d, PHOTXMAXi, PHOTXMINi, TOPTXi, EPSMIN, EPSD, & 
    OPTNP, LXLN, LXHN, KNi, KIi, KXMIN, LXH, LXL, MXH, MXL, KPi, WPKX, NFIXi) result (res)
    double precision, intent(in):: light_extinction_sum, temp, depth, ired, n, p, x_i, d, PHOTXMAXi, PHOTXMINi, TOPTXi, & 
    EPSMIN, EPSD, OPTNP, LXLN, LXHN, KNi, KIi, KXMIN, LXH, LXL, MXH, MXL, KPi, WPKX, NFIXi
    double precision:: phoxti, eps, iredz, phoxli, kxn, phoxnsi, kx, phoxpi, phoxni, res

    phoxti = lin_dep(PHOTXMAXi, PHOTXMINi, TOPTXi, temp) + PHOTXMINi ! 9.7
    
    eps = EPSMIN + light_extinction_sum + EPSD * d ! 7.2

    iredz = 1 / (eps * depth) * log((ired + KIi) / (KIi + ired * exp(-eps * depth))) ! 7.0 + 9.8 and modified for avg light
    
    IF ((n/p) < OPTNP) THEN  ! 9.8
        phoxli = iredz * (1 - NFIXi)
    ELSE
        phoxli = iredz
    END IF

    IF (n <= WPKX * OPTNP) THEN ! 9.12
        kxn = LXLN * N**MXL
    ELSE
        kxn = KXMIN + LXHN * N**MXH
    END IF

    IF (x_i /= 0d0) THEN
        phoxni = n / (x_i * (KNi/kxn + n/kxn + KNi/x_i + n/x_i)) ! 9.11
    ELSE
        phoxni = 0d0
    END IF

    IF (p <= WPKX) THEN ! 9.10
        kx = LXL * p**MXL
    ELSE
        kx = KXMIN + LXH * p**MXH
    END IF
    
    IF (x_i /= 0d0) THEN
        phoxpi = p / (x_i * (KPi/kx + p/kx + KPi/x_i + p/x_i)) ! 9.9
    ELSE
        phoxpi = 0d0
    END IF

    IF (n/p <= OPTNP .AND. NFIXi < 1d-4) THEN ! 9.6
        phoxnsi = phoxni
    ELSE
        phoxnsi = phoxpi
    END IF
    
    res = phoxti * phoxli * phoxnsi ! 9.5
end function

! ingestion rate of phytoplankton group i by zoo
function f_g_i (other_x_sum, sum_other_hwgi, x_number, g, temp, x_i, z, d, nx, PFCi, GMAX, GMIN, R, TOPTZ, &
    PFXi, KPFi, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, PF) result (res)
    double precision, intent(in):: other_x_sum, sum_other_hwgi, x_number, g, temp, x_i, z, d, nx, PFCi, &
    GMAX, GMIN, R, TOPTZ, PFXi, KPFi, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, PF
    double precision:: hwgi_result, hwgd_result, res
    
    hwgi_result = f_hwg_i(other_x_sum, x_number, temp, x_i, z, nx, PFCi, PFXi, KPFi, WPKZ, LGL, MGL, KZMIN, &
    LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ) ! 9.21.1

    hwgd_result = f_hwgd(temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ) ! 9.21.2

    res = hwgi_result / ((sum_other_hwgi + hwgi_result) + hwgd_result) * g ! 9.19.1
end function

! ingestion rate of detritus by zoo
function f_gd (sum_hwgi, g, temp, z, d, GMAX, GMIN, R, TOPTZ, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, PF) result (res)
    double precision, intent(in):: sum_hwgi, g, temp, z, d, GMAX, GMIN, R, TOPTZ, WPKZ, LGL, MGL, KZMIN, &
    LGH, MGH, KXG, PF
    double precision:: hwgd_result, res

    hwgd_result = f_hwgd(temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ) ! 9.21.2

    res = hwgd_result / (sum_hwgi + hwgd_result) * g ! 9.19.2
end function

! ingestion rate dependence on temperature
function f_gdt (temp, GMAX, GMIN, R, TOPTZ) result (res)
    double precision, intent(in):: temp, GMAX, GMIN, R, TOPTZ
    double precision:: res

    res = (GMAX - GMIN) * (EXP(-R * ABS(LOG(temp / TOPTZ)))) + GMIN ! 9.22
end function

! auxillary value for ingestion rate calculation of phytoplankton group i by zoo
function f_hwg_i (other_x_sum, x_number, temp, x_i, z, nx, PFCi, PFXi, KPFi, WPKZ, LGL, MGL, KZMIN, LGH, &
    MGH, KXG, GMAX, GMIN, R, TOPTZ) result (res)
    double precision, intent(in):: other_x_sum, x_number, temp, x_i, z, nx, PFCi, PFXi, KPFi, WPKZ, LGL, &
    MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ
    double precision:: pfi_result, kzi, hwgdbi_result, res

    IF (x_i /= 0d0) THEN
        pfi_result = f_pf_i(other_x_sum, x_number, nx, PFCi, PFXi, KPFi)

        IF (pfi_result * x_i <= WPKZ) THEN ! 9.24.1
            kzi = LGL * (x_i * pfi_result) ** MGL
        ELSE
            kzi = KZMIN + LGH * (x_i * pfi_result) ** MGH
        END IF

        hwgdbi_result = x_i * pfi_result / (z * (KXG/kzi + x_i/kzi + KXG/z + x_i/z)) ! 9.23.1

        res = hwgdbi_result * f_gdt(temp, GMAX, GMIN, R, TOPTZ) ! 9.21.1
    ELSE
        res = 0d0
    END IF
end function

! auxillary value for ingestion rate calculation of detritus by zoo
function f_hwgd (temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ) result (res)
    double precision, intent(in):: temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ
    double precision:: kzd, hwgdbd_result, res
    
    IF (d * PF <= WPKZ) THEN ! 9.24.2
        kzd = LGL * (d * PF) ** MGL
    ELSE
        kzd = KZMIN + LGH * (d * PF) ** MGH
    END IF

    hwgdbd_result = d * PF / (z * (KXG/kzd + d/kzd + KXG/z + d/z)) ! 9.23.2

    res = hwgdbd_result * f_gdt(temp, GMAX, GMIN, R, TOPTZ) ! 9.21.2
end function

! preference factor for ingestion of phytoplankton group i by zoo
function f_pf_i (other_x_sum, x_number, nx, PFCi, PFXi, KPFi) result (res)
    double precision, intent(in):: other_x_sum, x_number, nx, PFCi, PFXi, KPFi
    double precision:: b3, res

    IF (x_number == nx) THEN ! 9.25
        res = PFCi
    ELSE
        b3 = other_x_sum - PFXi

        IF (b3 <= 0d0) THEN
            res = 1d0
        ELSE
            IF (KPFi - PFXi /= 0d0) THEN
                res = 1 / (b3 * (1 / (KPFi - PFXi) + 1 / b3))
            ELSE
                res = 1d0
            END IF
        END IF
    END IF
end function

! overall ingestion rate of zoo
function f_g (xi_pfi_sum, temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ) result (res)
    double precision, intent(in):: xi_pfi_sum, temp, z, d, PF, WPKZ, LGL, MGL, KZMIN, LGH, MGH, KXG, GMAX, GMIN, R, TOPTZ
    double precision:: gdb, kz, pref_factor, res

    IF ((xi_pfi_sum + d * PF) <= WPKZ) THEN ! 9.31
        kz = LGL * (xi_pfi_sum + d * PF) ** MGL
    ELSE
        kz = KZMIN + LGH * (xi_pfi_sum + d * PF) ** MGH
    END IF

    pref_factor = xi_pfi_sum + d * PF
    
    gdb = pref_factor / (z * (KXG/kz + pref_factor/kz + KXG/z + pref_factor/z)) ! 9.30

    res = f_gdt(temp, GMAX, GMIN, R, TOPTZ) * gdb ! 9.29
end function

! reproduction coefficient of zoo
function f_egg (temp, DTA, DTB, DTC, DTMIN) result (res)
    double precision, intent(in):: temp, DTA, DTB, DTC, DTMIN
    double precision:: dt, res

    dt = EXP(DTA - DTB * LOG(temp) - DTC * ((LOG(temp)) ** 2)) ! 12.3

    IF (dt >= DTMIN) THEN ! 12.2
        res = DTMIN / dt
    ELSE
        res = 1d0
    END IF
end function

! respiration rate of zoo
function f_rz (g, temp, RZOPT, RZMIN, GMAX, RZTMIN, TOPTZ) result (res)
    double precision, intent(in):: g, temp, RZOPT, RZMIN, GMAX, RZTMIN, TOPTZ
    double precision:: rzg, rzt, res

    rzg = (lin_dep(RZOPT, RZMIN, GMAX, g) + RZMIN ) * (1 / RZOPT) ! 12.8

    rzt = (RZOPT - RZTMIN) * (temp / TOPTZ) ** 2 + RZTMIN ! 12.9

    res = rzg * rzt ! 12.7
end function

! oxygen saturation value (we assume always saturation due to shallow lake and constant mixing)
function f_o (time, temp_rise) result (res)
    double precision, intent(in):: time, temp_rise
    double precision:: abs_temp, res

    abs_temp = i_temp(time, temp_rise) + 273.15

    res = EXP(-139.34411 + 1.575701d5 / abs_temp - 6.642308d7 / (abs_temp ** 2) + 1.2438d10 / (abs_temp ** 3) &
    - 8.621949d11 / (abs_temp ** 4)) ! 15.1
end function

! oxygen consumption by sedimenting phytoplankton
function f_lo (sum_xsedi, g, temp, vol, depth, ased, o, d, KMINER, VD, SEZMAX, KSEZA, YOX, RZOPT, RZMIN, GMAX, RZTMIN, &
     TOPTZ, KO) result (res)
    double precision, intent(in):: sum_xsedi, g, temp, vol, depth, o, d, KMINER, VD, SEZMAX, KSEZA, YOX, ased, RZOPT, &
    RZMIN, GMAX, RZTMIN, TOPTZ, KO
    double precision:: minerd_value, minerd, sum_xdsed, seza, lsez, rz_result, olim, res

    olim = monod(1d0, KO, o)

    minerd_value = KMINER * depth / VD * olim ! 16.8.2

    IF (minerd_value > 1) THEN
        minerd = 1d0
    ELSE
        minerd = minerd_value
    END IF

    sum_xdsed = sum_xsedi * olim + (VD * d * minerd / depth) ! 16.7

    seza = SEZMAX * exp(0.08 * temp) * o / (KSEZA / o) ! 16.10

    lsez = seza * YOX * ased / vol ! 16.9

    rz_result = f_rz(g, temp, RZOPT, RZMIN, GMAX, RZTMIN, TOPTZ)

    res = sum_xdsed + rz_result * depth * olim + lsez * olim ! 16.6
end function

! sedimentation rate of phytoplankton group i
function f_xsed_i (x_i, depth, VSi, KMINER) result (res)
    double precision, intent(in):: x_i, depth, VSi, KMINER
    double precision:: mineri, res

    mineri = KMINER * depth / VSi ! 16.8.1

    IF (mineri > 1) THEN
        mineri = 1d0
    END IF

    res = VSi * x_i * mineri / depth
end function

! Helper functions

function lin_dep (max, min, opt, val) result (res)
    double precision, intent(in):: max, min, opt, val
    double precision:: res

    res = (max - min) / opt * val
end function

function monod (max_rate, half_sat, val) result (res)
    double precision, intent(in):: max_rate, half_sat, val
    double precision:: res

    res = max_rate * val / (half_sat + val)
end function

! Input values

! lake volume
function i_vol (time) result (res)
    double precision, intent(in):: time
    double precision:: res

    res = VE(time)
end function

! lake depth
function i_depth (time) result (res)
    double precision, intent(in):: time
    double precision:: res

    res = ZMIX(time)
end function

! sediment area (assumed as = lake area)
function i_ased (time) result (res)
    double precision, intent(in):: time
    double precision:: res

    res = VE(time) / ZMIX(time)
end function

! dilution rate
function i_qperve (time, qperve_factor) result (res)
    double precision, intent(in):: time, qperve_factor
    double precision:: res

    res = (QIN(time) / (VE(time) + QIN(time))) * qperve_factor
end function

! photosynthetic active radiation (PAR), 50% of global light
function i_ired (time, RL, RLW, iin_factor) result (res)
    double precision, intent(in):: time, RL, RLW, iin_factor
    double precision:: res

    IF (i_ice(time) == 1d0) THEN
        res = IIN(time) * RLW * iin_factor
    ELSE
        res = IIN(time) * RL * iin_factor
    END IF

end function

! temperature
function i_temp (time, temp_rise) result (res)
    double precision, intent(in):: time, temp_rise
    double precision:: temp_result
    double precision:: res

    temp_result = TE(time) + (time / 36525d-2 * temp_rise)
    
    IF (temp_result < 0d0) THEN
        res = 0d0
    ELSE
        res = temp_result
    END IF
end function

! N inflow
function i_ninflow (time, nin_factor) result (res)
    double precision, intent(in):: time, nin_factor
    double precision:: res

    res = NIN(time) * nin_factor
end function

! P inflow
function i_pinflow (time, pin_factor) result (res)
    double precision, intent(in):: time, pin_factor
    double precision:: res

    res = PIN(time) * pin_factor
end function

! detritus inflow
function i_dinflow (time, din_factor) result (res)
    double precision, intent(in):: time, din_factor
    double precision:: res

    res = POMIN(time) * din_factor
end function

! ice cover (not used currently)
function i_ice (time) result (res)
    double precision, intent(in):: time
    double precision:: res

    res = stagnation(time)
end function

function f_apsfmax (time, apsfmax_switch, APSFMAX) result (res)
    double precision, intent(in):: time, apsfmax_switch, APSFMAX
    double precision:: year, res

    IF (apsfmax_switch == 1d0) THEN
        year = ceiling(time / 36525d-2)
        
        IF (year == 1) THEN
            res = APSFMAX
        ELSE IF (year == 2) THEN
            res = 1d-1
        ELSE IF (year < 5) THEN
            res = (APSFMAX / 4) * (year - 2)
        ELSE
            res = APSFMAX
        END IF
    ELSE
        res = APSFMAX
    END IF
end function

end module