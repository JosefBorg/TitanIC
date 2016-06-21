##################################################################################################

#TitanIC

rm(list=ls())
options(java.parameters = "-Xmx4000m")

#Call_Packages
library(xlsx)

#Constants
s=5.670e-8 				#Stefan_Boltzmann_Constant_Wm^-2K^-4
r=6.960e8 				#Current_4.55Gyr_Sun_Radius_m (Grayzeck, E.; NASA, 2013)
R=1.422e12 				#Current_4.55Gyr_Titan_Distance_From_Sun_m (Coustenis et al.; 2008)
h=6.626e-34				#Planck's_Constant_Js
c=2.998e8				#Speed_of_Light_in_Vacuum_ms^-1
K_b=1.38e-23			#Boltzmann_Constant_JK^-1
CST=5779				#Current_Sun_Temperature_K
Avog=6.023e23			#Avogadro's_Constant
TR=2.575e6				#Titan_Radius_m
mbar=100				#mbar_in_Pascals
barn=1e-28				#Barn_to_metres^2

#RMMs
C=12.0107
H=1.00794
O=15.9994
N=14.0067
Ar=39.948

#Current_Sun_Luminosity
k=s*CST^4 				#Stefan_Boltzmann_Law_Current_Radiant_Output_Wm^-2
L=k*4*pi*r^2 			#Current_4.55Gyr_Total_Luminosity_Sun_W

#Full_Sun_Timeline (Sackmann, J., 1993)
Time_full <- c(0.048, 4.55, 7.56, 9.37, 10.91, 11.64, 12.088, 12.15, 12.233, 12.233, 12.234, 12.239, 12.316, 12.344, 12.345, 12.365066, 12.365355, 12.365446)
Lum_full <- c(0.70*L, 1.00*L, 1.33*L, 1.67*L, 2.21*L, 2.73*L, 17.3*L, 34*L, 2349*L, 57.7*L, 41*L, 45.9*L, 42.4*L, 110*L, 130*L, 2999*L, 5190*L, 90*L)
SunR_full <- c(0.90*r, 1.00*r, 1.13*r, 1.275*r, 1.575*r, 2.30*r, 6.38*r, 9.5*r, 165.8*r, 12.0*r, 9.5*r, 10.3*r, 9.4*r, 17.6*r, 20*r, 180.3*r, 177.0*r, 0.058*r)
TempSun_full <- c(5586, 5779, 5843, 5819, 6517, 4902, 4664, 4540, 3107, 4595, 4724, 4688, 4819, 4453, 4375, 3160, 3660)
 

#Sun_Timeline_Present_till_Before_Tip_of_RGB_and_Pulsations
Time_0 <- c(4.55, 7.56, 9.37, 10.91, 11.64, 12.088, 12.15)
Lum_0 <- c(1.00*L, 1.33*L, 1.67*L, 2.21*L, 2.73*L, 17.3*L, 34*L)
SunR_0 <- c(1.00*r, 1.13*r, 1.275*r, 1.575*r, 2.30*r, 6.38*r, 9.5*r)
TempSun_0 <- c(5779, 5843, 5819, 6517, 4902, 4664, 4540)

##################################################################################################
##################################################################################################

#Sun_Timeline_1_from_4.55Gyr_till_7.56Gyr
Time_1 <- c(4.5500, 7.5600)
Lum_1 <- c(1.00*L, 1.33*L)
SunR_1 <- c(1.00*r, 1.13*r)
TempSun_1 <- c(5779, 5843)
Timestep_1 <- seq(4.5500, 7.5600, 0.0001)

#Radiance_Titan_Timestep_1
j_1=Lum_1/(4*pi*(R-(SunR_1-r))^2) 					#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_1
m_Rad_1=(j_1[2]-j_1[1])/(Time_1[2]-Time_1[1])
y_Rad_1=(m_Rad_1*(Timestep_1) - m_Rad_1*Time_1[1] + j_1[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_1=data.frame(Timestep_1, y_Rad_1)

#write.xlsx(x = (d_Rad_1), file = "Titan_Rad_Timestep_1.xlsx",
#        sheetName = "Rad_Timestep_1", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_1
m_TempSun_1=(TempSun_1[2]-TempSun_1[1])/(Time_1[2]-Time_1[1])
y_TempSun_1=(m_TempSun_1*(Timestep_1) - m_TempSun_1*Time_1[1] + TempSun_1[1])	

d_TempSun_1=data.frame(Timestep_1, y_TempSun_1)

#write.xlsx(x = (d_TempSun_1), file = "TempSun_Timestep_1.xlsx",
#        sheetName = "TempSun_Timestep_1", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Radius_at_Timestep_1
m_SunR_1=(SunR_1[2]-SunR_1[1])/(Time_1[2]-Time_1[1])
y_SunR_1=(m_SunR_1*(Timestep_1) - m_SunR_1*Time_1[1] + SunR_1[1])	

d_SunR_1=data.frame(Timestep_1, y_SunR_1)

#write.xlsx(x = (d_SunR_1), file = "SunR_Timestep_1.xlsx",
#        sheetName = "SunR_Timestep_1", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_1
m_Lum_1=(Lum_1[2]-Lum_1[1])/(Time_1[2]-Time_1[1])
y_Lum_1=(m_Lum_1*(Timestep_1) - m_Lum_1*Time_1[1] + Lum_1[1])	

d_Lum_1=data.frame(Timestep_1, y_Lum_1)

#write.xlsx(x = (d_Lum_1), file = "Lum_Timestep_1.xlsx",
#        sheetName = "Lum_Timestep_1", row.names = FALSE, col.names = TRUE)

##################################################################################################
##################################################################################################

#Sun_Timeline_2_from_7.56Gyr_till_9.37Gyr
Time_2 <- c(7.5601, 9.3700)
Lum_2 <- c(1.33*L, 1.67*L)
SunR_2 <- c(1.13*r, 1.275*r)
TempSun_2 <- c(5843, 5819)
Timestep_2 <- seq(7.5601, 9.3700, 0.0001)

#Radiance_Titan_Timestep_2
j_2=Lum_2/(4*pi*(R-(SunR_2-r))^2) 		#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_2
m_Rad_2=(j_2[2]-j_2[1])/(Time_2[2]-Time_2[1])
y_Rad_2=(m_Rad_2*(Timestep_2) - m_Rad_2*Time_2[1] + j_2[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_2=data.frame(Timestep_2, y_Rad_2)

#write.xlsx(x = (d_Rad_2), file = "Titan_Rad_Timestep_2.xlsx",
#        sheetName = "Rad_Timestep_2", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_2
m_TempSun_2=(TempSun_2[2]-TempSun_2[1])/(Time_2[2]-Time_2[1])
y_TempSun_2=(m_TempSun_2*(Timestep_2) - m_TempSun_2*Time_2[1] + TempSun_2[1])	

d_TempSun_2=data.frame(Timestep_2, y_TempSun_2)

#write.xlsx(x = (d_TempSun_2), file = "TempSun_Timestep_2.xlsx",
#        sheetName = "TempSun_Timestep_2", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Radius_at_Timestep_2
m_SunR_2=(SunR_2[2]-SunR_2[1])/(Time_2[2]-Time_2[1])
y_SunR_2=(m_SunR_2*(Timestep_2) - m_SunR_2*Time_2[1] + SunR_2[1])	

d_SunR_2=data.frame(Timestep_2, y_SunR_2)

#write.xlsx(x = (d_SunR_2), file = "SunR_Timestep_2.xlsx",
#        sheetName = "SunR_Timestep_2", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_2
m_Lum_2=(Lum_2[2]-Lum_2[1])/(Time_2[2]-Time_2[1])
y_Lum_2=(m_Lum_2*(Timestep_2) - m_Lum_2*Time_2[1] + Lum_2[1])	

d_Lum_2=data.frame(Timestep_2, y_Lum_2)

#write.xlsx(x = (d_Lum_2), file = "Lum_Timestep_2.xlsx",
#        sheetName = "Lum_Timestep_2", row.names = FALSE, col.names = TRUE)  

###################################################################################################
###################################################################################################

#Sun_Timeline_3_from_9.37Gyr_till_10.91Gyr
Time_3 <- c(9.3701, 10.9100)
Lum_3 <- c(1.67*L, 2.21*L)
SunR_3 <- c(1.275*r, 1.575*r)
TempSun_3 <- c(5819, 6517)
Timestep_3 <- seq(9.3701, 10.9100, 0.0001)

#Radiance_Titan_Timestep_3
j_3=Lum_3/(4*pi*(R-(SunR_3-r))^2) 		#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_3
m_Rad_3=(j_3[2]-j_3[1])/(Time_3[2]-Time_3[1])
y_Rad_3=(m_Rad_3*(Timestep_3) - m_Rad_3*Time_3[1] + j_3[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_3=data.frame(Timestep_3, y_Rad_3)

#write.xlsx(x = (d_Rad_3), file = "Titan_Rad_Timestep_3.xlsx",
#        sheetName = "Rad_Timestep_3", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_3
m_TempSun_3=(TempSun_3[2]-TempSun_3[1])/(Time_3[2]-Time_3[1])
y_TempSun_3=(m_TempSun_3*(Timestep_3) - m_TempSun_3*Time_3[1] + TempSun_3[1])

d_TempSun_3=data.frame(Timestep_3, y_TempSun_3)

#write.xlsx(x = (d_TempSun_3), file = "TempSun_Timestep_3.xlsx",
#        sheetName = "TempSun_Timestep_3", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Radius_at_Timestep_3
m_SunR_3=(SunR_3[2]-SunR_3[1])/(Time_3[2]-Time_3[1])
y_SunR_3=(m_SunR_3*(Timestep_3) - m_SunR_3*Time_3[1] + SunR_3[1])

d_SunR_3=data.frame(Timestep_3, y_SunR_3)

#write.xlsx(x = (d_SunR_3), file = "SunR_Timestep_3.xlsx",
#        sheetName = "SunR_Timestep_3", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_3
m_Lum_3=(Lum_3[2]-Lum_3[1])/(Time_3[2]-Time_3[1])
y_Lum_3=(m_Lum_3*(Timestep_3) - m_Lum_3*Time_3[1] + Lum_3[1])

d_Lum_3=data.frame(Timestep_3, y_Lum_3)

#write.xlsx(x = (d_Lum_3), file = "Lum_Timestep_3.xlsx",
#        sheetName = "Lum_Timestep_3", row.names = FALSE, col.names = TRUE)    

###################################################################################################
###################################################################################################

#Sun_Timeline_4_from_10.91Gyr_till_11.64Gyr
Time_4 <- c(10.9101, 11.6400)
Lum_4 <- c(2.21*L, 2.73*L)
SunR_4 <- c(1.575*r, 2.30*r)
TempSun_4 <- c(6517, 4902)
Timestep_4 <- seq(10.9101, 11.6400, 0.0001)

#Radiance_Titan_Timestep_4
j_4=Lum_4/(4*pi*(R-(SunR_4-r))^2) 		#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_4
m_Rad_4=(j_4[2]-j_4[1])/(Time_4[2]-Time_4[1])
y_Rad_4=(m_Rad_4*(Timestep_4) - m_Rad_4*Time_4[1] + j_4[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_4=data.frame(Timestep_4, y_Rad_4)

#write.xlsx(x = (d_Rad_4), file = "Titan_Rad_Timestep_4.xlsx",
#        sheetName = "Rad_Timestep_4", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_4
m_TempSun_4=(TempSun_4[2]-TempSun_4[1])/(Time_4[2]-Time_4[1])
y_TempSun_4=(m_TempSun_4*(Timestep_4) - m_TempSun_4*Time_4[1] + TempSun_4[1])

d_TempSun_4=data.frame(Timestep_4, y_TempSun_4)

#write.xlsx(x = (d_TempSun_4), file = "TempSun_Timestep_4.xlsx",
#        sheetName = "TempSun_Timestep_4", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Radius_at_Timestep_4
m_SunR_4=(SunR_4[2]-SunR_4[1])/(Time_4[2]-Time_4[1])
y_SunR_4=(m_SunR_4*(Timestep_4) - m_SunR_4*Time_4[1] + SunR_4[1])	

d_SunR_4=data.frame(Timestep_4, y_SunR_4)

#write.xlsx(x = (d_SunR_4), file = "SunR_Timestep_4.xlsx",
#        sheetName = "SunR_Timestep_4", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_4
m_Lum_4=(Lum_4[2]-Lum_4[1])/(Time_4[2]-Time_4[1])
y_Lum_4=(m_Lum_4*(Timestep_4) - m_Lum_4*Time_4[1] + Lum_4[1])	

d_Lum_4=data.frame(Timestep_4, y_Lum_4)

#write.xlsx(x = (d_Lum_4), file = "Lum_Timestep_4.xlsx",
#        sheetName = "Lum_Timestep_4", row.names = FALSE, col.names = TRUE)

###################################################################################################
###################################################################################################

#Sun_Timeline_5_from_11.64Gyr_till_12.088Gyr
Time_5 <- c(11.6401, 12.0880)
Lum_5 <- c(2.73*L, 17.3*L)
SunR_5 <- c(2.30*r, 6.38*r)
TempSun_5 <- c(4902, 4664)
Timestep_5 <- seq(11.6401, 12.0880, 0.00001)

#Radiance_Titan_Timestep_5
j_5=Lum_5/(4*pi*(R-(SunR_5-r))^2) 		#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_5
m_Rad_5=(j_5[2]-j_5[1])/(Time_5[2]-Time_5[1])
y_Rad_5=(m_Rad_5*(Timestep_5) - m_Rad_5*Time_5[1] + j_5[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_5=data.frame(Timestep_5, y_Rad_5)

#write.xlsx(x = (d_Rad_5), file = "Titan_Rad_Timestep_5.xlsx",
#        sheetName = "Rad_Timestep_5", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_5
m_TempSun_5=(TempSun_5[2]-TempSun_5[1])/(Time_5[2]-Time_5[1])
y_TempSun_5=(m_TempSun_5*(Timestep_5) - m_TempSun_5*Time_5[1] + TempSun_5[1])

d_TempSun_5=data.frame(Timestep_5, y_TempSun_5)

#write.xlsx(x = (d_TempSun_5), file = "TempSun_Timestep_5.xlsx",
#        sheetName = "TempSun_Timestep_5", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Radius_at_Timestep_5
m_SunR_5=(SunR_5[2]-SunR_5[1])/(Time_5[2]-Time_5[1])
y_SunR_5=(m_SunR_5*(Timestep_5) - m_SunR_5*Time_5[1] + SunR_5[1])	

d_SunR_5=data.frame(Timestep_5, y_SunR_5)

#write.xlsx(x = (d_SunR_5), file = "SunR_Timestep_5.xlsx",
#        sheetName = "SunR_Timestep_5", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_5
m_Lum_5=(Lum_5[2]-Lum_5[1])/(Time_5[2]-Time_5[1])
y_Lum_5=(m_Lum_5*(Timestep_5) - m_Lum_5*Time_5[1] + Lum_5[1])	

d_Lum_5=data.frame(Timestep_5, y_Lum_5)

#write.xlsx(x = (d_Lum_5), file = "Lum_Timestep_5.xlsx",
#        sheetName = "Lum_Timestep_5", row.names = FALSE, col.names = TRUE)

##################################################################################################
##################################################################################################

#Sun_Timeline_6_from_12.088Gyr_till_12.15Gyr
Time_6 <- c(12.0881, 12.1500)
Lum_6 <- c(17.3*L, 34*L)
SunR_6 <- c(6.38*r, 9.5*r)
TempSun_6 <- c(4664, 4540)
Timestep_6 <- seq(12.0881, 12.1500, 0.00001)

#Radiance_Titan_Timestep_6
j_6=Lum_6/(4*pi*(R-(SunR_6-r))^2) 		#Time_Series_Solar_Flux_Titan_Wm^-2

#########################################################

#Equation_Determining_Radiance_at_Timestep_6
m_Rad_6=(j_6[2]-j_6[1])/(Time_6[2]-Time_6[1])
y_Rad_6=(m_Rad_6*(Timestep_6) - m_Rad_6*Time_6[1] + j_6[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Rad_6=data.frame(Timestep_6, y_Rad_6)

#write.xlsx(x = (d_Rad_6), file = "Titan_Rad_Timestep_6.xlsx",
#        sheetName = "Rad_Timestep_6", row.names = FALSE, col.names = TRUE)

#########################################################

#Equation_Determining_Sun_Temperature_at_Timestep_6
m_TempSun_6=(TempSun_6[2]-TempSun_6[1])/(Time_6[2]-Time_6[1])
y_TempSun_6=(m_TempSun_6*(Timestep_6) - m_TempSun_6*Time_6[1] + TempSun_6[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_TempSun_6=data.frame(Timestep_6, y_TempSun_6)

#write.xlsx(x = (d_TempSun_6), file = "TempSun_Timestep_6.xlsx",
#        sheetName = "TempSun_Timestep_6", row.names = FALSE, col.names = TRUE)

########################################################

#Equation_Determining_Sun_Radius_at_Timestep_6
m_SunR_6=(SunR_6[2]-SunR_6[1])/(Time_6[2]-Time_6[1])
y_SunR_6=(m_SunR_6*(Timestep_6) - m_SunR_6*Time_6[1] + SunR_6[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_SunR_6=data.frame(Timestep_6, y_SunR_6)

#write.xlsx(x = (d_SunR_6), file = "SunR_Timestep_6.xlsx",
#        sheetName = "SunR_Timestep_6", row.names = FALSE, col.names = TRUE)

########################################################

#Equation_Determining_Sun_Luminosity_at_Timestep_6
m_Lum_6=(Lum_6[2]-Lum_6[1])/(Time_6[2]-Time_6[1])
y_Lum_6=(m_Lum_6*(Timestep_6) - m_Lum_6*Time_6[1] + Lum_6[1])	#Solar_Flux_Titan_at_Timesteps_Wm^-2

d_Lum_6=data.frame(Timestep_6, y_Lum_6)

#write.xlsx(x = (d_Lum_6), file = "Lum_Timestep_6.xlsx",
#        sheetName = "Lum_Timestep_6", row.names = FALSE, col.names = TRUE)

##################################################################################################
##################################################################################################

#Define_Relevant_Timelines

Timestep_all <- c(Timestep_1, Timestep_2, Timestep_3, Timestep_4, Timestep_5, Timestep_6)
y_Rad_all <- c(y_Rad_1, y_Rad_2, y_Rad_3, y_Rad_4, y_Rad_5, y_Rad_6)
y_TempSun_all <- c(y_TempSun_1, y_TempSun_2, y_TempSun_3, y_TempSun_4, y_TempSun_5, y_TempSun_6)
y_SunR_all <- c(y_SunR_1, y_SunR_2, y_SunR_3, y_SunR_4, y_SunR_5, y_SunR_6)
y_Lum_all <- c(y_Lum_1, y_Lum_2, y_Lum_3, y_Lum_4, y_Lum_5, y_Lum_6)

##################################################################################################
##################################################################################################

#Define_Solid_Angle_of_Sun_at_Titan

Omega=((pi*(y_SunR_all^2))/(R-(y_SunR_all-r))^2)

##################################################################################################
##################################################################################################

#Calculating_Irradiance_and_Actinic_Flux_on_Titan

#Assuming_Albedo_30%_(Still_needs_to_be_calculated)

Alb=0.3
theta=0

#######################################################

#For_Collimated_Light
 
Irrad_Collim=(y_Rad_all*(cos(theta))*Omega)
ActFlux_Collim=(y_Rad_all*Omega)

#######################################################

#For_Isotropic_Light

Irrad_Isotrop=(pi*y_Rad_all)
ActFlux_Isotrop=(2*pi*y_Rad_all)

#######################################################

#For_Reflected_Light

Irrad_Reflec=Alb*(Irrad_Collim+Irrad_Isotrop)
ActFlux_Reflec=Alb*((2*cos(theta)*ActFlux_Collim) + ActFlux_Isotrop)

#######################################################

#Total_Actinic_Flux_F

Irrad_Total=(Irrad_Collim+Irrad_Isotrop)
Tot_Act_Flux=(ActFlux_Collim+ActFlux_Isotrop) #Not_including_reflected_since_Lyman_alpha_radiation_is_not_reflected

##################################################################################################
##################################################################################################

#Calculating_Sun_Fractions_of_W_m^-2_of_Different_Wavelengths

#B=((2*h*c^2)/(w^5))*(1/((exp((h*c)/(w*K_b*T)))-1))				#Planck's_Law

T=y_TempSun_all

FracB_Rad_G_X_UV=NULL
FracB_Rad_VIS=NULL
FracB_Rad_IR1=NULL
FracB_Rad_IR2=NULL
FracB_Rad_IR3=NULL
FracB_Rad_IR4=NULL
FracB_Rad_IR5=NULL
FracB_Rad_IR6=NULL
FracB_Rad_IR7=NULL
FracB_Rad_IR8=NULL
FracB_Rad_RW=NULL

IR_all=c(8.33e-7, 2.17e-6, 2.44e-6, 3.13e-6, 3.57e-6, 6.67e-6, 8.33e-6, 1e-2)

for (n in 1:121882)
{
integrand <- function(lambda) {((2*h*c^2)/(lambda^5))*(1/((exp((h*c)/(lambda*K_b*T[n])))-1))}
Blambda0=integrate(integrand, lower=0, upper=1e-12)$val
Blambda1=integrate(integrand, lower=1e-12, upper=1e-11)$val
Blambda2=integrate(integrand, lower=1e-11, upper=1e-10)$val
Blambda3=integrate(integrand, lower=1e-10, upper=1e-9)$val
Blambda4=integrate(integrand, lower=1e-9, upper=1e-8)$val
Blambda5=integrate(integrand, lower=1e-8, upper=1e-7)$val
Blambda6=integrate(integrand, lower=1e-7, upper=4e-7)$val
Blambda7=integrate(integrand, lower=4e-7, upper=7e-7)$val
Blambda8=integrate(integrand, lower=7e-7, upper=IR_all[1])$val
Blambda9=integrate(integrand, lower=IR_all[1], upper=IR_all[2])$val
Blambda10=integrate(integrand, lower=IR_all[2], upper=IR_all[3])$val
Blambda11=integrate(integrand, lower=IR_all[3], upper=IR_all[4])$val
Blambda12=integrate(integrand, lower=IR_all[4], upper=IR_all[5])$val
Blambda13=integrate(integrand, lower=IR_all[5], upper=IR_all[6])$val
Blambda14=integrate(integrand, lower=IR_all[6], upper=IR_all[7])$val
Blambda15=integrate(integrand, lower=IR_all[7], upper=IR_all[8])$val
Blambda16=integrate(integrand, lower=1e-2, upper=1e-1)$val
Blambda17=integrate(integrand, lower=1e-1, upper=1)$val
Blambda18=integrate(integrand, lower=1, upper=1e1)$val
Blambda19=integrate(integrand, lower=1e1, upper=1e2)$val
Blambda20=integrate(integrand, lower=1e2, upper=1e3)$val
Blambda21=integrate(integrand, lower=1e3, upper=1e4)$val
Blambda22=integrate(integrand, lower=1e4, upper=1e5)$val
Blambda23=integrate(integrand, lower=1e5, upper=1e6)$val
Blambda_all <- c(Blambda0, Blambda1, Blambda2, Blambda3, Blambda4, Blambda5, Blambda6, Blambda7, Blambda8, Blambda9, Blambda10, Blambda11, Blambda12, Blambda13, Blambda14, Blambda15, Blambda16, Blambda17, Blambda18, Blambda19, Blambda20, Blambda21, Blambda22, Blambda23)
FracB_Rad_G_X_UV[n]=(((Blambda0+Blambda1+Blambda2+Blambda3+Blambda4+Blambda5+Blambda6)/(sum(Blambda_all)))) 
FracB_Rad_VIS[n]=(((Blambda7)/(sum(Blambda_all)))) 
FracB_Rad_IR1[n]=(((Blambda8)/(sum(Blambda_all))))
FracB_Rad_IR2[n]=(((Blambda9)/(sum(Blambda_all)))) 
FracB_Rad_IR3[n]=(((Blambda10)/(sum(Blambda_all)))) 
FracB_Rad_IR4[n]=(((Blambda11)/(sum(Blambda_all)))) 
FracB_Rad_IR5[n]=(((Blambda12)/(sum(Blambda_all))))
FracB_Rad_IR6[n]=(((Blambda13)/(sum(Blambda_all))))
FracB_Rad_IR7[n]=(((Blambda14)/(sum(Blambda_all))))
FracB_Rad_IR8[n]=(((Blambda15)/(sum(Blambda_all))))  
FracB_Rad_RW[n]=(((Blambda16+Blambda17+Blambda18+Blambda19+Blambda20+Blambda21+Blambda22+Blambda23)/(sum(Blambda_all)))) 
}
B_GXUV_Vec=c(FracB_Rad_G_X_UV[1:121882])
B_VIS_Vec=c(FracB_Rad_VIS[1:121882])
B_IR1_Vec=c(FracB_Rad_IR1[1:121882])
B_IR2_Vec=c(FracB_Rad_IR2[1:121882])
B_IR3_Vec=c(FracB_Rad_IR3[1:121882])
B_IR4_Vec=c(FracB_Rad_IR4[1:121882])
B_IR5_Vec=c(FracB_Rad_IR5[1:121882])
B_IR6_Vec=c(FracB_Rad_IR6[1:121882])
B_IR7_Vec=c(FracB_Rad_IR7[1:121882])
B_IR8_Vec=c(FracB_Rad_IR8[1:121882])
B_RW_Vec=c(FracB_Rad_RW[1:121882])

#d_Sun_Rad <- data.frame(B_GXUV_Vec, B_VIS_Vec, B_IR_Vec, B_RW_Vec)

#write.xlsx(x = (d_Sun_Rad), file = "Sun_Rad_all.xlsx",
#        sheetName = "Sun_Rad_all", row.names = FALSE, col.names = TRUE)

##########################################

F_Titan_G_X_UV=B_GXUV_Vec*(Irrad_Total[1:121882])
F_Titan_VIS=B_VIS_Vec*(Irrad_Total[1:121882])
F_Titan_IR=(B_IR1_Vec)*(Irrad_Total[1:121882])
F_Titan_IR2=(B_IR2_Vec)*(Irrad_Total[1:121882])
F_Titan_IR3=(B_IR3_Vec)*(Irrad_Total[1:121882])
F_Titan_IR4=(B_IR4_Vec)*(Irrad_Total[1:121882])
F_Titan_IR5=(B_IR5_Vec)*(Irrad_Total[1:121882])
F_Titan_IR6=(B_IR6_Vec)*(Irrad_Total[1:121882])
F_Titan_IR7=(B_IR7_Vec)*(Irrad_Total[1:121882])
F_Titan_IR8=(B_IR8_Vec)*(Irrad_Total[1:121882])
F_Titan_RW=B_RW_Vec*(Irrad_Total[1:121882])

##################################################################################################
##################################################################################################

#Calculating_Photon_Density_at_Lyman_Alpha_Wavelength_121.6nm_as_in_Ribas_2005

FLyman_ergs_cm=19.2*(Timestep_all^(-0.72)) 					#ergs_s^-1_cm^-2_Sun
FLyman_W_cm=FLyman_ergs_cm*1e-7							#W_cm^-2_Sun
FLyman_W_m=FLyman_W_cm*1e4								#W_m^-2_Sun
FLyman_all=FLyman_W_m*(4*pi*((y_SunR_all)^2))					#W_Sun
FracLyman_all=FLyman_all/y_Lum_all							#Fraction_of_Sun_Luminosity_Lyman_Alpha

FLyman_Titan_all=FracLyman_all*Tot_Act_Flux					#W_m^-2_Titan			

############################################

E_LymanPhoton=((h*c)/(1.216e-7))

Lyman_Photon_Dens=FLyman_Titan_all/E_LymanPhoton				#Number_of_Lyman_photons_per_m^2
Lyman_Photon_Dens_cm=Lyman_Photon_Dens/10000					#Number_of_Lyman_photons_per_cm^2	
 
########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################

#Titan_Surface_Temp_Boltzmann_Approximation

y_TempTitan_all=(y_TempSun_all)*(((y_SunR_all)/(2*(R-(y_SunR_all-r))))^(1/2))

##################################################################################################

#Calculating_Reflected_Fractions_of_W_m^-2_of_Different_Wavelengths

#B=((2*h*c^2)/(w^5))*(1/((exp((h*c)/(w*K_b*T)))-1))				#Planck's_Law

T=y_TempTitan_all

FracB_Rad_G_X_UVRef=NULL
FracB_Rad_VISRef=NULL
FracB_Rad_IR1Ref=NULL
FracB_Rad_IR2Ref=NULL
FracB_Rad_IR3Ref=NULL
FracB_Rad_IR4Ref=NULL
FracB_Rad_IR5Ref=NULL
FracB_Rad_IR6Ref=NULL
FracB_Rad_IR7Ref=NULL
FracB_Rad_IR8Ref=NULL
FracB_Rad_RWRef=NULL

IR_all=c(8.33e-7, 2.17e-6, 2.44e-6, 3.13e-6, 3.57e-6, 6.67e-6, 8.33e-6, 1e-2)  

for (n in 1:121882)
{
integrand <- function(lambda) {((2*h*c^2)/(lambda^5))*(1/((exp((h*c)/(lambda*K_b*T[n])))-1))}
Blambda0=integrate(integrand, lower=0, upper=1e-12)$val
Blambda1=integrate(integrand, lower=1e-12, upper=1e-11)$val
Blambda2=integrate(integrand, lower=1e-11, upper=1e-10)$val
Blambda3=integrate(integrand, lower=1e-10, upper=1e-9)$val
Blambda4=integrate(integrand, lower=1e-9, upper=1e-8)$val
Blambda5=integrate(integrand, lower=1e-8, upper=1e-7)$val
Blambda6=integrate(integrand, lower=1e-7, upper=4e-7)$val
Blambda7=integrate(integrand, lower=4e-7, upper=7e-7)$val
Blambda8=integrate(integrand, lower=7e-7, upper=IR_all[1])$val
Blambda9=integrate(integrand, lower=IR_all[1], upper=IR_all[2])$val
Blambda10=integrate(integrand, lower=IR_all[2], upper=IR_all[3])$val
Blambda11=integrate(integrand, lower=IR_all[3], upper=IR_all[4])$val
Blambda12=integrate(integrand, lower=IR_all[4], upper=IR_all[5])$val
Blambda13=integrate(integrand, lower=IR_all[5], upper=IR_all[6])$val
Blambda14=integrate(integrand, lower=IR_all[6], upper=IR_all[7])$val
Blambda15=integrate(integrand, lower=IR_all[7], upper=IR_all[8])$val
Blambda16=integrate(integrand, lower=1e-2, upper=1e-1)$val
Blambda17=integrate(integrand, lower=1e-1, upper=1)$val
Blambda18=integrate(integrand, lower=1, upper=1e1)$val
Blambda19=integrate(integrand, lower=1e1, upper=1e2)$val
Blambda20=integrate(integrand, lower=1e2, upper=1e3)$val
Blambda21=integrate(integrand, lower=1e3, upper=1e4)$val
Blambda22=integrate(integrand, lower=1e4, upper=1e5)$val
Blambda23=integrate(integrand, lower=1e5, upper=1e6)$val
Blambda_all <- c(Blambda0, Blambda1, Blambda2, Blambda3, Blambda4, Blambda5, Blambda6, Blambda7, Blambda8, Blambda9, Blambda10, Blambda11, Blambda12, Blambda13, Blambda14, Blambda15, Blambda16, Blambda17, Blambda18, Blambda19, Blambda20, Blambda21, Blambda22, Blambda23)
FracB_Rad_G_X_UVRef[n]=(((Blambda0+Blambda1+Blambda2+Blambda3+Blambda4+Blambda5+Blambda6)/(sum(Blambda_all)))) 
FracB_Rad_VISRef[n]=(((Blambda7)/(sum(Blambda_all)))) 
FracB_Rad_IR1Ref[n]=(((Blambda8)/(sum(Blambda_all))))
FracB_Rad_IR2Ref[n]=(((Blambda9)/(sum(Blambda_all)))) 
FracB_Rad_IR3Ref[n]=(((Blambda10)/(sum(Blambda_all)))) 
FracB_Rad_IR4Ref[n]=(((Blambda11)/(sum(Blambda_all)))) 
FracB_Rad_IR5Ref[n]=(((Blambda12)/(sum(Blambda_all))))
FracB_Rad_IR6Ref[n]=(((Blambda13)/(sum(Blambda_all))))
FracB_Rad_IR7Ref[n]=(((Blambda14)/(sum(Blambda_all))))
FracB_Rad_IR8Ref[n]=(((Blambda15)/(sum(Blambda_all))))  
FracB_Rad_RWRef[n]=(((Blambda16+Blambda17+Blambda18+Blambda19+Blambda20+Blambda21+Blambda22+Blambda23)/(sum(Blambda_all)))) 
}
B_GXUVRef_Vec=c(FracB_Rad_G_X_UVRef[1:121882])
B_VISRef_Vec=c(FracB_Rad_VISRef[1:121882])
B_IR1Ref_Vec=c(FracB_Rad_IR1Ref[1:121882])
B_IR2Ref_Vec=c(FracB_Rad_IR2Ref[1:121882])
B_IR3Ref_Vec=c(FracB_Rad_IR3Ref[1:121882])
B_IR4Ref_Vec=c(FracB_Rad_IR4Ref[1:121882])
B_IR5Ref_Vec=c(FracB_Rad_IR5Ref[1:121882])
B_IR6Ref_Vec=c(FracB_Rad_IR6Ref[1:121882])
B_IR7Ref_Vec=c(FracB_Rad_IR7Ref[1:121882])
B_IR8Ref_Vec=c(FracB_Rad_IR8Ref[1:121882])
B_RWRef_Vec=c(FracB_Rad_RWRef[1:121882])

###############################################################################################
##################################################################################################

#Titan_Maria_and_Lakes

Total_Lakes_Vol=7e100				#Volume_of_Lakes_in_m^3
Lake_Perc_Frac_SA=0.011			 	#Hayes_et_al_2014_Conference

##########################################

#Calculating_Lake_SA_m^2

Titan_SA=4*pi*(TR^2)				#Titan_Surface_Area_in_m^2
Lake_SA=Lake_Perc_Frac_SA*Titan_SA		#Surface_Area_of_Lakes_in_m^2

##########################################

#Calculating_Global_Liquid_Methane_Moles

#Composition_Molar_Fractions			#Cordier_et_al_2014_Conference

Nitrogen=4.90e-3
Methane=9.69e-2
Argon=5.01e-6
Carbon_Monoxide=4.21e-7
Ethane=7.64e-1
Propane=7.42e-2
Butane=1.39e-2
Hydrogen=3.99e-11

#RMM_Lake_Compounds				#http://www.caslab.com/List-of-Elements/
RMM_N2=(2*N)
RMM_CH4=(1*C)+(4*H)
RMM_Ar=Ar
RMM_CO=(1*C)+(1*O)
RMM_C2H6=(2*C)+(6*H)
RMM_C3H8=(3*C)+(8*H)
RMM_C4H10=(4*C)+(10*H)
RMM_H2=(2*H)

#Per_mole_of_Titan_sea
Mass_N2=Nitrogen*RMM_N2
Mass_CH4=Methane*RMM_CH4
Mass_Ar=Argon*RMM_Ar
Mass_CO=Carbon_Monoxide*RMM_CO
Mass_C2H6=Ethane*RMM_C2H6
Mass_C3H8=Propane*RMM_C3H8
Mass_C4H10=Butane*RMM_C4H10
Mass_H2=Hydrogen*RMM_H2

#Density_Lake_Compounds				#liquid_g_cm^-3
Dens_N2=0.808
Dens_CH4=0.4534
Dens_Ar=1.3954
Dens_CO=0.789
Dens_C2H6=0.6541
Dens_C3H8=0.58088
Dens_C4H10=0.60126
Dens_H2=0.07805

#Volume_Fraction
Vol_N2=Mass_N2/Dens_N2
Vol_CH4=Mass_CH4/Dens_CH4
Vol_Ar=Mass_Ar/Dens_Ar
Vol_CO=Mass_CO/Dens_CO
Vol_C2H6=Mass_C2H6/Dens_C2H6
Vol_C3H8=Mass_C3H8/Dens_C3H8
Vol_C4H10=Mass_C4H10/Dens_C4H10
Vol_H2=Mass_H2/Dens_H2
Vol_Perc_CH4=((Vol_CH4)/(Vol_N2+Vol_CH4+Vol_Ar+Vol_CO+Vol_C2H6+Vol_C3H8+Vol_C4H10+Vol_H2))*100

#Total_Volume_Methane
Methane_Volume=(Vol_Perc_CH4/100)*Total_Lakes_Vol	#Volume_of_Methane_in_Lakes_in_m^3
Methane_Volume_cm=Methane_Volume*1e6			#Volume_of_Methane_in_Lakes_in_cm^3

#Total_Mass_Methane
Methane_Mass=Dens_CH4*Methane_Volume_cm			#Mass_of_Methane_in_Lakes_in_g

#Total_Moles_Methane
Methane_Moles=Methane_Mass/RMM_CH4				#Moles_of_Methane_in_Lakes

#Total_Moles_Lake
Total_Moles_Lake=(1/Methane)*Methane_Moles		#Moles_of_all_Lake_Constituents

#########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################
#########################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################

#Calculating_Photolysis_of_Methane_taking into_account;
	#Methane_Gas_Absorption_of_Lyman_Wavelength_Photons_with_Altitude_(Including_Methane_Altitude_Profile_1300km_to_Surface)
	#Methane_Surface_Changes_Evaporation_and_Rainfall
	#Detached_and_Main_Haze_Layer_Particle_Scattering_and_Absorption_IN_PROCESS

###Dissociations
###CH4->CH3+H
###CH4->CH2+H2
###CH4->CH+H2+H

################################Declaring_Values################################

############################Null_Vector_Declarations############################

#####For_Actinic_Flux_Differences,_Photolysis_and_Diffusion

F=NULL
CH4_Timestep_Vecv1_n=NULL
F_n=NULL
F_N=NULL
F_N_Diff=NULL
CH4_Timestep_Vecv1=NULL
F_0=NULL
CH4_n_Timestep=NULL
Total_n_Vec=NULL
CH4_Timestep_n=NULL
F_Rayl_N2=NULL
F_Rayl_H2=NULL
Pressure=NULL
Actual_Pressure=NULL
Photolysis_Alt=NULL
CH4_Aerosol_Prod=NULL
Diffusion_Coefficient=NULL
Diff=NULL
Diff_mol=NULL
Ratio_Diff=NULL
Diffusion_Actual_Ratio=NULL
Actual_Methane_Increase=NULL
FIR=NULL
FIR_2=NULL
FIR_3=NULL
FIR_4=NULL
FIR_5=NULL
FIR_6=NULL
FIR_7=NULL
FIR_8=NULL
FIR_0=NULL
FIR_2_0=NULL
FIR_3_0=NULL
FIR_4_0=NULL
FIR_5_0=NULL
FIR_6_0=NULL
FIR_7_0=NULL
FIR_8_0=NULL
FIR_Ref=NULL
FIR_Ref_2=NULL
FIR_Ref_3=NULL
FIR_Ref_4=NULL
FIR_Ref_5=NULL
FIR_Ref_6=NULL
FIR_Ref_7=NULL
FIR_Ref_8=NULL
FIR_Ref_0=NULL
FIR_Ref_2_0=NULL
FIR_Ref_3_0=NULL
FIR_Ref_4_0=NULL
FIR_Ref_5_0=NULL
FIR_Ref_6_0=NULL
FIR_Ref_7_0=NULL
FIR_Ref_8_0=NULL
F_IRRef_1=NULL
F_IRRef_2=NULL
F_IRRef_3=NULL
F_IRRef_4=NULL
F_IRRef_5=NULL
F_IRRef_6=NULL
F_IRRef_7=NULL
F_IRRef_8=NULL
FIR_intervals=NULL
FIR_Ref_intervals=NULL
FIR_Total=NULL
Temp_Increase=NULL
Temp_Increase_Altitude=NULL
FIR_Altitude=NULL
FIR_Ref_Altitude=NULL
FIR_Total_Altitude=NULL
Temp_Increase_Altitude=NULL
Titan_Temperature_Altitude=NULL
Titan_Surf_Temperature=NULL

FracB_Rad_G_X_UVRef=NULL
FracB_Rad_VISRef=NULL
FracB_Rad_IR1Ref=NULL
FracB_Rad_IR2Ref=NULL
FracB_Rad_IR3Ref=NULL
FracB_Rad_IR4Ref=NULL
FracB_Rad_IR5Ref=NULL
FracB_Rad_IR6Ref=NULL
FracB_Rad_IR7Ref=NULL
FracB_Rad_IR8Ref=NULL
FracB_Rad_RWRef=NULL

#####For_Evaporation_and_Precipitation_Balance

CH4_Perc_Surf=NULL
CH4_n_surface=NULL
CH4_moles_surface=NULL
q_CH4=NULL
qstar_CH4=NULL
E=NULL
E_year=NULL
E_CH4_global_year=NULL
E_CH4_global_year_g=NULL
E_moles_CH4=NULL
Moles_CH4_Increase_cm=NULL
Moles_CH4_after_1yr=NULL
CH4_n_surface_after_1yr=NULL
Increase_in_n_after_1yr=NULL
Init_CH4_Perc=NULL
Total_n_surface=NULL
Methane_CH4=NULL
Total_Mol_Lake=NULL
Methane_Mol=NULL
CH4_Perc_Surf_Vec=NULL
CH4_Perc_Surf_Vec_2=NULL
Visc_all=NULL
z_0_all=NULL
Kcoeff=NULL
pstar_CH4=NULL
Methane_CH4=NULL
Init_CH4_Perc=NULL
Total_n_surface=NULL
Total_Mol_Lake=NULL
Methane_Mol=NULL
Per_trop_layer=NULL
Increase_in_n_after_1_yr_actual=NULL
Photolysis_year=NULL
Actual_Pressure_Total_Vec=NULL
Rain=NULL
	
#####################Constants_and_Sequences########################

#####Altitude_Number_Densities_of_Gases_and_Particles
	#Photolysis_Constants_also_Added

Altitude_Vec_1 <- seq(200, 1300, 5)
Altitude_Vec_2 <- seq(6, 198, 2)
Altitude_Vec_3=c(5, 4, 3, 2, 1.5, 1, 0.5)
Altitude_Vec_all=c(Altitude_Vec_1[221:1], Altitude_Vec_2[97:1], Altitude_Vec_3[1:7])
n_Total_Vec=c(2.86e6, 3.05e6, 3.26e6, 3.48e6, 3.72e6, 3.98e6, 4.25e6, 4.55e6, 4.86e6, 5.20e6, 5.57e6, 5.96e6, 6.38e6, 6.83e6, 7.32e6, 7.84e6, 8.40e6, 9.01e6, 9.66e6, 1.04e7, 1.11e7, 1.19e7, 1.28e7, 1.37e7, 1.47e7, 1.58e7, 1.70e7, 1.83e7, 1.96e7, 2.11e7, 2.27e7, 2.44e7, 2.63e7, 2.83e7, 3.04e7, 3.27e7, 3.52e7, 3.80e7, 4.09e7, 4.41e7, 4.75e7, 5.12e7, 5.52e7, 5.95e7, 6.42e7, 6.93e7, 7.48e7, 8.07e7, 8.72e7, 9.41e7, 1.02e8, 1.10e8, 1.19e8, 1.28e8, 1.39e8, 1.50e8, 1.62e8, 1.76e8, 1.90e8, 2.06e8, 2.23e8, 2.41e8, 2.61e8, 2.83e8, 3.07e8, 3.33e8, 3.61e8, 3.91e8, 4.25e8, 4.61e8, 5.00e8, 5.43e8, 5.89e8, 6.40e8, 6.95e8, 7.56e8, 8.21e8, 8.93e8, 9.72e8, 1.06e9, 1.15e9, 1.26e9, 1.37e9, 1.49e9, 1.63e9, 1.78e9, 1.94e9, 2.13e9, 2.33e9, 2.55e9, 2.79e9, 3.06e9, 3.35e9, 3.68e9, 4.05e9, 4.45e9, 4.89e9, 5.39e9, 5.94e9, 6.55e9, 7.23e9, 7.99e9, 8.84e9, 9.78e9, 1.08e10, 1.20e10, 1.33e10, 1.48e10, 1.65e10, 1.84e10, 2.05e10, 2.28e10, 2.55e10, 2.85e10, 3.19e10, 3.58e10, 4.01e10, 4.51e10, 5.07e10, 5.71e10, 6.44e10, 7.27e10, 8.21e10, 9.29e10, 1.05e11, 1.19e11, 1.36e11, 1.54e11, 1.75e11, 2.00e11, 2.28e11, 2.61e11, 2.98e11, 3.41e11, 3.91e11, 4.49e11, 5.16e11, 5.93e11, 6.82e11, 7.85e11, 9.04e11, 1.04e12, 1.20e12, 1.39e12, 1.60e12, 1.85e12, 2.13e12, 2.46e12, 2.83e12, 3.27e12, 3.76e12, 4.33e12, 4.98e12, 5.72e12, 6.56e12, 7.52e12, 8.60e12, 9.84e12, 1.12e13, 1.28e13, 1.46e13, 1.66e13, 1.89e13, 2.14e13, 2.43e13, 2.75e13, 3.12e13, 3.52e13, 3.98e13, 4.49e13, 5.07e13, 5.71e13, 6.43e13, 7.23e13, 8.13e13, 9.14e13, 1.03e14, 1.15e14, 1.29e14, 1.45e14, 1.63e14, 1.83e14, 2.05e14, 2.29e14, 2.57e14, 2.88e14, 3.23e14, 3.62e14, 4.06e14, 4.56e14, 5.11e14, 5.72e14, 6.41e14, 7.17e14, 8.04e14, 9.01e14, 1.01e15, 1.13e15, 1.27e15, 1.42e15, 1.60e15, 1.80e15, 2.02e15, 2.27e15, 2.55e15, 2.87e15, 3.24e15, 3.65e15, 4.11e15, 4.64e15, 5.24e15, 5.92e15, 6.70e15, 7.58e15, 8.59e15, 9.74e15, 1.10e16, 1.25e16, 1.43e16, 1.62e16, 1.85e16, 1.95e16, 2.05e16, 2.16e16, 2.28e16, 2.40e16, 2.54e16, 2.67e16, 2.82e16, 2.98e16, 3.14e16, 3.31e16, 3.50e16, 3.69e16, 3.90e16, 4.12e16, 4.35e16, 4.60e16, 4.86e16, 5.13e16, 5.43e16, 5.74e16, 6.06e16, 6.41e16, 6.78e16, 7.18e16, 7.60e16, 8.30e16, 8.53e16, 9.10e16, 9.67e16, 1.02e17, 1.09e17, 1.16e17, 1.23e17, 1.30e17, 1.39e17, 1.49e17, 1.58e17, 1.69e17, 1.81e17, 1.93e17, 2.06e17, 2.19e17, 2.34e17, 2.50e17, 2.67e17, 2.86e17, 3.01e17, 3.30e17, 3.55e17, 3.81e17, 4.10e17, 4.43e17, 4.80e17, 5.19e17, 5.58e17, 6.06e17, 6.65e17, 7.32e17, 8.06e17, 8.93e17, 9.90e17, 1.10e18, 1.25e18, 1.44e18, 1.68e18, 2.00e18, 2.42e18, 2.88e18, 3.38e18, 3.95e18, 4.58e18, 5.26e18, 6.01e18, 6.86e18, 7.82e18, 8.90e18, 1.01e19, 1.15e19, 1.31e19, 1.48e19, 1.68e19, 1.91e19, 2.16e19, 2.45e19, 2.76e19, 3.11e19, 3.50e19, 3.91e19, 4.39e19, 4.89e19, 5.44e19, 6.04e19, 6.69e19, 7.40e19, 8.14e19, 8.93e19, 9.32e19, 9.74e19, 1.01e20, 1.05e20, 1.06e20, 1.08e20, 1.10e20)
n_CH4_Frac_Alt_Vec=c(0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.022, 0.022, 0.022, 0.022, 0.022, 0.022, 0.022, 0.022, 0.022, 0.022, 0.0178, 0.0178, 0.0178, 0.0178, 0.0178, 0.0178, 0.0178, 0.0178, 0.0178, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0131, 0.0149, 0.0149, 0.0153, 0.0153, 0.0149, 0.0149, 0.0151, 0.0151, 0.0151, 0.0151, 0.0151, 0.0149, 0.0149, 0.0146, 0.0146, 0.0148, 0.0148, 0.0146, 0.0146, 0.0146, 0.0145, 0.0145, 0.0145, 0.0145, 0.0146, 0.0146, 0.0146, 0.0146, 0.0146, 0.0146, 0.0147, 0.0147, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0149, 0.0150, 0.0150, 0.0153, 0.0153, 0.0153, 0.0164, 0.0164, 0.0164, 0.0180, 0.0180, 0.0202, 0.0233, 0.0233, 0.0260, 0.0283, 0.0336, 0.0397, 0.0439, 0.0499, 0.0536, 0.0555, 0.0575, 0.0576, 0.0572, 0.0573, 0.0573, 0.0567) #Cui_et_al_(2009),_Niemann_et_al_(2010),_Extrapolated
n_H2_Frac_Alt_Vec=c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
n_N2_Frac_Alt_Vec=1-(n_CH4_Frac_Alt_Vec+n_H2_Frac_Alt_Vec)

n_CH4_Alt_Vec=n_CH4_Frac_Alt_Vec*n_Total_Vec
Init_CH4_Alt_Vec=n_CH4_Frac_Alt_Vec*n_Total_Vec

n_H2_Alt_Vec=n_H2_Frac_Alt_Vec*n_Total_Vec
n_N2_Alt_Vec=n_N2_Frac_Alt_Vec*n_Total_Vec

n_CH4=n_CH4_Alt_Vec
Moles_CH4=n_CH4/Avog							#Moles_CH4_per_cm^3

sigma=2e-17
sigma_IR_all=c(8.00e-28, 6.41e-23, 1.63e-21, 1.42e-22, 2.91e-20, 1.03e-22, 1.70e-20, 5.00e-29)
phi=0.98
CS_N=5.37	#Rayleigh_Cross_Area_barn
CS_H=2.28	#Rayleigh_Cross_Area_barn

#####Evaporation_Calculations_Constants
	#Assuming_that_Surface_Area_of_Lakes_remains_constant
	#E=Dens_Air_Surf*Kcoeff*(qstar_CH4 - q_CH4)*Wind_speed

Dens_Air_Surf=((((n_CH4_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_CH4)+(((n_N2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_H2))*1e3			#kg_m^-3
Wind_speed=1						#m_s^-1
Actual_Pressure_Total_Vec[325]=144754	#in_Pascals
Karman=0.40						#von_Karman_constant
Scal_Corr=1						#Scalar_Correction
z_r=1							#Reference_height
Charnock=0.035					#Charnock's_constant
g_Tit=1.35						#Gravity_Surface_Titan_m_s^-2
Fric_vel=0.035					#Friction_Velocity_m_s^-1
A_CH4_N2=7.34e16					#Wilson_et_al_2013
A_CH4_H2=2.3e17					#Wilson_et_al_2013
s_CH4_N2=0.750					#Wilson_et_al_2013
s_CH4_H2=0.765					#Wilson_et_al_2013

g=1
Init_CH4_Perc[1]=n_CH4_Frac_Alt_Vec[325]
Init_CH4_Perc[g]=n_CH4_Frac_Alt_Vec[325]
Total_n_surface[1]=n_Total_Vec[325]												#Yelle et al.
Total_n_surface[g]=n_Total_Vec[325]
Methane_CH4[1]=9.69e-2
Methane_CH4[g]=9.69e-2
Total_Mol_Lake[1]=Total_Moles_Lake
Total_Mol_Lake[g]=Total_Moles_Lake
Methane_Mol[1]=Methane_Moles
Methane_Mol[g]=Methane_Moles
CH4_Timestep_n[325]=n_CH4_Alt_Vec[325]
CH4_Aerosol_Prod2=6.539043e-12

	#Assuming_Surface_Atm_Layer_0.5km

Vol_Surface_Atm_Layer=((4/3)*pi*((2.619e6)^3))-((4/3)*pi*((2.575e6)^3))				#Volume_Surface_Atmosphere_Layer_m^3
Vol_Surface_Atm_Layer_cm=Vol_Surface_Atm_Layer*1e6

#########

Actual_PathL_m=NULL
Actual_PathL_m=2.575e6+(Altitude_Vec_all*1000)
Actual_PathL_m[326]=2.575e6

Vol_Atm_Layer=NULL
Vol_Atm_Layer_cm=NULL
for (n in 1:325)
{
Vol_Atm_Layer[n]=((4/3)*pi*((Actual_PathL_m[n])^3))-((4/3)*pi*((Actual_PathL_m[n+1])^3))				#Volume_Surface_Atmosphere_Layer_m^3
Vol_Atm_Layer_cm[n]=Vol_Atm_Layer[n]*1e6
}

##########################

#Rainfall_assuming_current_20mm_constant_lake_surface_all_methane

Rainfall_Constant=90.62073

Global_Rain_m=(sum(Moles_CH4[299:325]))*Rainfall_Constant
Global_Rain_m3=Global_Rain_m*(Lake_SA)*0.01
Global_Rain_cm3=Global_Rain_m3*1e6
Mass_Global_Rain=Global_Rain_cm3*Dens_CH4
Moles_Global_Rain=Mass_Global_Rain/RMM_CH4									#Moles_of_rain_global_h^-1
Moles_Global_Rain_yr=Moles_Global_Rain*(1e5)									#Moles_of_rain_global_yr^-1
Moles_Global_Rain_cm=Moles_Global_Rain/Vol_Surface_Atm_Layer_cm						#Moles_of_rain_cm^3_h^-1
Moles_CH4_Decrease_cm=Moles_Global_Rain_cm								#Volume_Surface_Atmosphere_Layer_cm^3

##########################

#######################################LOOP_START############################################

for (n in 1:50829)
{

#Infrared

for (y in 2:222)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)

FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=500000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
}

for (y in 223:319)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=200000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

for (y in 320:323)
{	
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=100000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 324:326)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=50000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

TotalF=(FIR[326]+FIR_2[326]+FIR_3[326]+FIR_4[326]+FIR_5[326]+FIR_6[326]+FIR_7[326]+FIR_8[326])

#Reflected_IR

F_IRRef_1[n]=B_IR1Ref_Vec[n]*TotalF*0.3
F_IRRef_2[n]=B_IR2Ref_Vec[n]*TotalF*0.3
F_IRRef_3[n]=B_IR3Ref_Vec[n]*TotalF*0.3
F_IRRef_4[n]=B_IR4Ref_Vec[n]*TotalF*0.3
F_IRRef_5[n]=B_IR5Ref_Vec[n]*TotalF*0.3
F_IRRef_6[n]=B_IR6Ref_Vec[n]*TotalF*0.3
F_IRRef_7[n]=B_IR7Ref_Vec[n]*TotalF*0.3
F_IRRef_8[n]=B_IR8Ref_Vec[n]*TotalF*0.3

for (y in 326:323)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]

FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=50000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 322:319)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=100000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 318:223)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=200000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 222:1)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=500000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

#Calculating_Absorbed_IR_with_altitude

#Irradiative_IR

for (y in 2:326)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)
FIR_intervals[y-1]=((FIR[y-1]-FIR[y])+(FIR_2[y-1]-FIR_2[y])+(FIR_3[y-1]-FIR_3[y])+(FIR_4[y-1]-FIR_4[y])+(FIR_5[y-1]-FIR_5[y])+(FIR_6[y-1]-FIR_6[y])+(FIR_7[y-1]-FIR_7[y])+(FIR_8[y-1]-FIR_8[y]))
}
	
#Reflected_IR

for (y in 2:326)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]
FIR_Ref_intervals[y-1]=((FIR_Ref[y]-FIR_Ref[y-1])+(FIR_Ref_2[y]-FIR_Ref_2[y-1])+(FIR_Ref_3[y]-FIR_Ref_3[y-1])+(FIR_Ref_4[y]-FIR_Ref_4[y-1])+(FIR_Ref_5[y]-FIR_Ref_5[y-1])+(FIR_Ref_6[y]-FIR_Ref_6[y-1])+(FIR_Ref_7[y]-FIR_Ref_7[y-1])+(FIR_Ref_8[y]-FIR_Ref_8[y-1]))
}

FIR_Total=FIR_intervals+FIR_Ref_intervals

Temp_Increase=(((FIR_Total*1e4)/s)^(1/4))

###########################################################

#Calculating_Actual_Titan_Surface_Temp

#Assuming_Albedo_30_percent

Actual_Titan_Surface_Temp=((0.7^(1/4))*y_TempTitan_all[n])+Temp_Increase

###########################################################################

#Lyman_Alpha

for (y in 2:221)
{
F[1]=Lyman_Photon_Dens_cm[n]									#Initial_4.55Gyr
F_0[y]=F[y-1]
Path_L=500000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 222:271)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_Particle_Scattering=F[y-1]*(CH4_Aerosol_Prod2/6.539043e-12)
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])			
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_Particle_Scattering+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 272:319)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 320:323)
{	
F_0[y]=F[y-1]
Path_L=100000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 324:325)
{
F_0[y]=F[y-1]
Path_L=50000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

##########################FACTORING_IN_EVAPORATION_AND_RAINFALL#########################################

#Loop_for_Range_Temp_<110K

A=6.34159															#Antoine_Equation_Constant_A_Prydz_and_Goodwin_1972
B=342.22														#Antoine_Equation_Constant_B_Prydz_and_Goodwin_1972
C=260.221														#Antoine_Equation_Constant_C_Prydz_and_Goodwin_1972

Init_CH4_Perc[n]=Init_CH4_Perc[g]
Methane_CH4[n]=Methane_CH4[g]
Total_Mol_Lake[n]=Total_Mol_Lake[g]
Methane_Mol[n]=Methane_Mol[g]

#Using_Antoine_Equation
pstar_CH4[n]=((10^(A-(B/(C+(Actual_Titan_Surface_Temp[325]-273.15)))))*100)				#SVP_Temp_Range_90K<_T_<110K

CH4_n_surface[n]=CH4_Timestep_n[325]										#per_cm^3
CH4_moles_surface[n]=CH4_n_surface[n]/Avog									#per_cm^3

q_CH4[n]=(0.573*((Methane_CH4[n]*(Init_CH4_Perc[n]*(Actual_Pressure_Total_Vec[325])))/(Actual_Pressure_Total_Vec[325])))			#Specific_Methane_Humidity
qstar_CH4[n]=(0.573*((Methane_CH4[n]*pstar_CH4[n])/(Actual_Pressure_Total_Vec[325])))									#Saturatation_Specific_Humidity_of_Methane_in_Mixture

#To_calculate_Kcoeff

Visc_all[n]=(1.718e-5 + (5.1e-8*(Actual_Titan_Surface_Temp[325] - 273)))
z_0_all[n]=(((Charnock*(Fric_vel^2))/g_Tit)+(0.11*(Visc_all[n]/(Dens_Air_Surf*Fric_vel))))
Kcoeff[n]=(((Karman*Scal_Corr)/(log(z_r/z_0_all[n])))^2)

#To_calculate_CH4_evaporation_rate_kg_m^-2_y^-1

E[n]=Dens_Air_Surf*Kcoeff[n]*(qstar_CH4[n] - q_CH4[n])*Wind_speed						#kg_m^-2_s^-1
E_year[n]=E[n]*3.15576e12												#kg_m^-2_yr^-1

#To_calculate_CH4_evaporation_rate_global_g_yr^-1

E_CH4_global_year[n]=E_year[n]*Lake_SA										#kg_yr^-1
E_CH4_global_year_g[n]=E_CH4_global_year[n]*1000								#g_yr^-1

#To_calculate_evaporation_rate_global_moles_CH4_yr^-1

E_moles_CH4[n]=E_CH4_global_year_g[n]/RMM_CH4									#mol_yr^-1

Increase_in_n_after_1yr[n]=E_moles_CH4[n]*Avog

##########################################################

#To_calculate_new_molar_fraction_CH4_in_lake

Total_Mol_Lake[g]=Total_Mol_Lake[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[g]=Methane_Mol[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[which(Methane_Mol<0)]=0
Methane_CH4[g]=Methane_Mol[g]/Total_Mol_Lake[g]
Methane_CH4[which(Methane_CH4<0)]=0

##############################################ENDING_EVAP##################################################

#Calculating_Diffusion_Methane_Vertical_Transport

Dens_Air=(((Moles_CH4)*RMM_CH4)+(((n_N2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_H2))*1e3
zeta_CH4=(Moles_CH4*Avog)/n_Total_Vec
zeta_H2=n_H2_Alt_Vec/n_Total_Vec
zeta_N2=n_N2_Alt_Vec/n_Total_Vec

D_CH4_N2=(A_CH4_N2*(Actual_Titan_Surface_Temp^s_CH4_N2))/Dens_Air
D_CH4_H2=(A_CH4_H2*(Actual_Titan_Surface_Temp^s_CH4_H2))/Dens_Air

Diff[1:325]=(1-zeta_CH4)/(((zeta_N2)/(D_CH4_N2)))

#Troposphere_and_Stratosphere

Ratio_Diff[1:325]=Diff[1:325]/(sum(Diff[1:325]))

Increase_in_n_after_1_yr_actual[299:325]=(Ratio_Diff[299:325]*Increase_in_n_after_1yr[n])/(Vol_Atm_Layer_cm[299:325])
Increase_in_n_after_1_yr_actual[1:298]=(Ratio_Diff[1:298]*Increase_in_n_after_1_yr_actual[299])/(Vol_Atm_Layer_cm[1:298])

############################################################

Moles_CH4[1:325]=Moles_CH4[1:325]+(Increase_in_n_after_1_yr_actual[1:325]/Avog)
Moles_CH4[299:325]=Moles_CH4[299:325]-(Moles_CH4_Decrease_cm)
Moles_CH4[which(Moles_CH4<0)]=0

J_CH4_1=(sigma*phi*F)
Photolysis=(J_CH4_1)*Moles_CH4								#Phot_Rate_per_second
Photolysis_year=(Photolysis*(3.15576e12))							#Phot_Rate_per_100000_years

CH4_0=Moles_CH4-Photolysis_year
CH4_0[which(CH4_0<0)]=0
CH4_Timestep_n=CH4_0*Avog
CH4_Timestep_n[which(CH4_Timestep_n<0)]=0

CH4_Timestep_Vecv1=((CH4_Timestep_n)/((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n)))
n_Total_Vec=((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n))

##########################################################

Init_CH4_Perc[g]=CH4_Timestep_n[325]/n_Total_Vec[325]						#New_Percentage_Methane_1yr

##########################################################

#Aerosol_Simulation_Assuming_Aerosols_Only_Produced_in_Pressures_<10Pa			#Coustenis_et_al_2006_Book

Actual_Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)	#Ideal_Gas_Laws

Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)		#Ideal_Gas_Laws
Pressure_Total_Vec[which(Pressure_Total_Vec<10)]=1
Pressure_Total_Vec[which(Pressure_Total_Vec>10)]=0

CH4_Aerosol_Prod2=sum(Photolysis_year[1:325]*Pressure_Total_Vec[1:325])

#####################################

Moles_CH4=CH4_0
n_CH4_Alt_Vec=CH4_Timestep_n
Dens_Air_Surf=((Moles_CH4[325]*RMM_CH4)+(((n_N2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_H2))*1e3			#kg_m^-3

####################################

#Rainfall_assuming_current_20mm_constant_lake_surface_all_methane

Rainfall_Constant=90.62073

Global_Rain_m=(sum(Moles_CH4[299:325]))*Rainfall_Constant
Global_Rain_m3=Global_Rain_m*(0.15*Titan_SA)
Global_Rain_cm3=Global_Rain_m3*1e6
Mass_Global_Rain=Global_Rain_cm3*Dens_CH4
Moles_Global_Rain=Mass_Global_Rain/RMM_CH4									#Moles_of_rain_global_h^-1
Moles_Global_Rain_yr=Moles_Global_Rain*(1e5)								#Moles_of_rain_global_yr^-1
Moles_Global_Rain_cm=Moles_Global_Rain/Vol_Surface_Atm_Layer_cm						#Moles_of_rain_cm^3_h^-1
Moles_CH4_Decrease_cm=Moles_Global_Rain_cm									#Moles_of_rain_cm^3_yr^-1			#Moles_of_rain_cm^3_yr^-1

####################################

Total_n_Vec[n]=data.frame(n_Total_Vec[1:325])
CH4_n_Timestep[n]=data.frame(CH4_Timestep_n[1:325])
CH4_Timestep_Vecv1_n[n]=data.frame(CH4_Timestep_Vecv1[1:325])
F_n[n]=data.frame(F[1:325])
Pressure[n]=data.frame(Pressure_Total_Vec[1:325])
Actual_Pressure[n]=data.frame(Actual_Pressure_Total_Vec[1:325])
Photolysis_Alt[n]=data.frame(Photolysis_year[1:325])
CH4_Aerosol_Prod[n]=CH4_Aerosol_Prod2
Diffusion_Coefficient[n]=data.frame(Diff[1:325])
Diffusion_Actual_Ratio[n]=data.frame(Ratio_Diff[1:325])
Actual_Methane_Increase[n]=data.frame(Increase_in_n_after_1_yr_actual[1:325])
FIR_Altitude[n]=data.frame(FIR[1:325]+FIR_2[1:325]+FIR_3[1:325]+FIR_4[1:325]+FIR_5[1:325]+FIR_6[1:325]+FIR_7[1:325]+FIR_8[1:325])
FIR_Ref_Altitude[n]=data.frame(FIR_Ref[1:325]+FIR_Ref_2[1:325]+FIR_Ref_3[1:325]+FIR_Ref_4[1:325]+FIR_Ref_5[1:325]+FIR_Ref_6[1:325]+FIR_Ref_7[1:325]+FIR_Ref_8[1:325])
FIR_Total_Altitude[n]=data.frame(FIR_Total[1:325])
Temp_Increase_Altitude[n]=data.frame(Temp_Increase[1:325])
Titan_Temperature_Altitude[n]=data.frame(Actual_Titan_Surface_Temp[1:325])
Titan_Surf_Temperature[n]=Actual_Titan_Surface_Temp[325]
Rain[n]=Moles_CH4_Decrease_cm

######################1_50829######################
}

Total_n_all=Total_n_Vec[1:50829]
Pressure_Alt=Pressure[1:50829]
Actual_Pressure_Alt=Actual_Pressure[1:50829]
CH4_n_all=CH4_n_Timestep[1:50829]
F_n_all=F_n[1:50829]
CH4_Timestep_Vec=CH4_Timestep_Vecv1_n[1:50829]
CH4_Evap_Rate_Vec=c(0, E_moles_CH4[1:50829])
CH4_Perc_Surf_Vec=c(0.0567, CH4_Perc_Surf[1:50829])
Photolysis_Vec=Photolysis_Alt[1:50829]
CH4_Aerosol_Prod_Vec=CH4_Aerosol_Prod[1:50829]
Diff_Coeff_Vec=Diffusion_Coefficient[1:50829]
Diff_Actual_Vec=Diffusion_Actual_Ratio[1:50829]
FIR_Altitude_Vec=FIR_Altitude[1:50829]
FIR_Ref_Altitude_Vec=FIR_Ref_Altitude[1:50829]
FIR_Total_Altitude_Vec=FIR_Total_Altitude[1:50829]
Temp_Increase_Altitude_Vec=Temp_Increase_Altitude[1:50829]
Titan_Temperature_Altitude_Vec=Titan_Temperature_Altitude[1:50829]
Titan_Surface_Temperature_Vec=Titan_Surf_Temperature[1:50829]

#############################################################################################################
#################################################LOOP_TRANSITION#############################################
#############################################################################################################

for (n in 50830:70901)
{

#Infrared

for (y in 2:222)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)

FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=500000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
}

for (y in 223:319)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=200000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

for (y in 320:323)
{	
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=100000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 324:326)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=50000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

TotalF=(FIR[326]+FIR_2[326]+FIR_3[326]+FIR_4[326]+FIR_5[326]+FIR_6[326]+FIR_7[326]+FIR_8[326])

#Reflected_IR

F_IRRef_1[n]=B_IR1Ref_Vec[n]*TotalF*0.3
F_IRRef_2[n]=B_IR2Ref_Vec[n]*TotalF*0.3
F_IRRef_3[n]=B_IR3Ref_Vec[n]*TotalF*0.3
F_IRRef_4[n]=B_IR4Ref_Vec[n]*TotalF*0.3
F_IRRef_5[n]=B_IR5Ref_Vec[n]*TotalF*0.3
F_IRRef_6[n]=B_IR6Ref_Vec[n]*TotalF*0.3
F_IRRef_7[n]=B_IR7Ref_Vec[n]*TotalF*0.3
F_IRRef_8[n]=B_IR8Ref_Vec[n]*TotalF*0.3

for (y in 326:323)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]

FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=50000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 322:319)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=100000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 318:223)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=200000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 222:1)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=500000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

#Calculating_Absorbed_IR_with_altitude

#Irradiative_IR

for (y in 2:326)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)
FIR_intervals[y-1]=((FIR[y-1]-FIR[y])+(FIR_2[y-1]-FIR_2[y])+(FIR_3[y-1]-FIR_3[y])+(FIR_4[y-1]-FIR_4[y])+(FIR_5[y-1]-FIR_5[y])+(FIR_6[y-1]-FIR_6[y])+(FIR_7[y-1]-FIR_7[y])+(FIR_8[y-1]-FIR_8[y]))
}
	
#Reflected_IR

for (y in 2:326)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]
FIR_Ref_intervals[y-1]=((FIR_Ref[y]-FIR_Ref[y-1])+(FIR_Ref_2[y]-FIR_Ref_2[y-1])+(FIR_Ref_3[y]-FIR_Ref_3[y-1])+(FIR_Ref_4[y]-FIR_Ref_4[y-1])+(FIR_Ref_5[y]-FIR_Ref_5[y-1])+(FIR_Ref_6[y]-FIR_Ref_6[y-1])+(FIR_Ref_7[y]-FIR_Ref_7[y-1])+(FIR_Ref_8[y]-FIR_Ref_8[y-1]))
}

FIR_Total=FIR_intervals+FIR_Ref_intervals

Temp_Increase=(((FIR_Total*1e4)/s)^(1/4))

###########################################################

#Calculating_Actual_Titan_Surface_Temp

#Assuming_Albedo_30_percent

Actual_Titan_Surface_Temp=((0.7^(1/4))*y_TempTitan_all[n])+Temp_Increase

###########################################################################

#Lyman_Alpha

for (y in 2:221)
{
F[1]=Lyman_Photon_Dens_cm[n]									#Initial_4.55Gyr
F_0[y]=F[y-1]
Path_L=500000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 222:271)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_Particle_Scattering=F[y-1]*(CH4_Aerosol_Prod2/6.539043e-12)
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])			
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_Particle_Scattering+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 272:319)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 320:323)
{	
F_0[y]=F[y-1]
Path_L=100000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 324:325)
{
F_0[y]=F[y-1]
Path_L=50000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

##########################FACTORING_IN_EVAPORATION_AND_RAINFALL#########################################

#Loop_for_Range_Temp_>110K

A=6.7021														#Antoine_Equation_Constant_A_Prydz_and_Goodwin_1972
B=394.48														#Antoine_Equation_Constant_B_Prydz_and_Goodwin_1972
C=264.609														#Antoine_Equation_Constant_C_Prydz_and_Goodwin_1972

Init_CH4_Perc[n]=Init_CH4_Perc[g]
Methane_CH4[n]=Methane_CH4[g]
Total_Mol_Lake[n]=Total_Mol_Lake[g]
Methane_Mol[n]=Methane_Mol[g]

#Using_Antoine_Equation
pstar_CH4[n]=((10^(A-(B/(C+(Actual_Titan_Surface_Temp[325]-273.15)))))*100)				#SVP_Temp_Range_90K<_T_<110K

CH4_n_surface[n]=CH4_Timestep_n[325]										#per_cm^3
CH4_moles_surface[n]=CH4_n_surface[n]/Avog									#per_cm^3

q_CH4[n]=(0.573*((Methane_CH4[n]*(Init_CH4_Perc[n]*(Actual_Pressure_Total_Vec[325])))/(Actual_Pressure_Total_Vec[325])))			#Specific_Methane_Humidity
qstar_CH4[n]=(0.573*((Methane_CH4[n]*pstar_CH4[n])/(Actual_Pressure_Total_Vec[325])))									#Saturatation_Specific_Humidity_of_Methane_in_Mixture

#To_calculate_Kcoeff

Visc_all[n]=(1.718e-5 + (5.1e-8*(Actual_Titan_Surface_Temp[325] - 273)))
z_0_all[n]=(((Charnock*(Fric_vel^2))/g_Tit)+(0.11*(Visc_all[n]/(Dens_Air_Surf*Fric_vel))))
Kcoeff[n]=(((Karman*Scal_Corr)/(log(z_r/z_0_all[n])))^2)

#To_calculate_CH4_evaporation_rate_kg_m^-2_y^-1

E[n]=Dens_Air_Surf*Kcoeff[n]*(qstar_CH4[n] - q_CH4[n])*Wind_speed						#kg_m^-2_s^-1
E_year[n]=E[n]*3.15576e12												#kg_m^-2_yr^-1

#To_calculate_CH4_evaporation_rate_global_g_yr^-1

E_CH4_global_year[n]=E_year[n]*Lake_SA										#kg_yr^-1
E_CH4_global_year_g[n]=E_CH4_global_year[n]*1000								#g_yr^-1

#To_calculate_evaporation_rate_global_moles_CH4_yr^-1

E_moles_CH4[n]=E_CH4_global_year_g[n]/RMM_CH4									#mol_yr^-1

Increase_in_n_after_1yr[n]=E_moles_CH4[n]*Avog

##########################################################

#To_calculate_new_molar_fraction_CH4_in_lake

Total_Mol_Lake[g]=Total_Mol_Lake[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[g]=Methane_Mol[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[which(Methane_Mol<0)]=0
Methane_CH4[g]=Methane_Mol[g]/Total_Mol_Lake[g]
Methane_CH4[which(Methane_CH4<0)]=0

##############################################ENDING_EVAP##################################################

#Calculating_Diffusion_Methane_Vertical_Transport

Dens_Air=(((Moles_CH4)*RMM_CH4)+(((n_N2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_H2))*1e3
zeta_CH4=(Moles_CH4*Avog)/n_Total_Vec
zeta_H2=n_H2_Alt_Vec/n_Total_Vec
zeta_N2=n_N2_Alt_Vec/n_Total_Vec

D_CH4_N2=(A_CH4_N2*(Actual_Titan_Surface_Temp^s_CH4_N2))/Dens_Air
D_CH4_H2=(A_CH4_H2*(Actual_Titan_Surface_Temp^s_CH4_H2))/Dens_Air

Diff[1:325]=(1-zeta_CH4)/(((zeta_N2)/(D_CH4_N2)))

#Troposphere_and_Stratosphere

Ratio_Diff[1:325]=Diff[1:325]/(sum(Diff[1:325]))

Increase_in_n_after_1_yr_actual[299:325]=(Ratio_Diff[299:325]*Increase_in_n_after_1yr[n])/(Vol_Atm_Layer_cm[299:325])
Increase_in_n_after_1_yr_actual[1:298]=(Ratio_Diff[1:298]*Increase_in_n_after_1_yr_actual[299])/(Vol_Atm_Layer_cm[1:298])

############################################################

Moles_CH4[1:325]=Moles_CH4[1:325]+(Increase_in_n_after_1_yr_actual[1:325]/Avog)
Moles_CH4[299:325]=Moles_CH4[299:325]-(Moles_CH4_Decrease_cm)
Moles_CH4[which(Moles_CH4<0)]=0

J_CH4_1=(sigma*phi*F)
Photolysis=(J_CH4_1)*Moles_CH4								#Phot_Rate_per_second
Photolysis_year=(Photolysis*(3.15576e12))							#Phot_Rate_per_100000_years

CH4_0=Moles_CH4-Photolysis_year
CH4_0[which(CH4_0<0)]=0
CH4_Timestep_n=CH4_0*Avog
CH4_Timestep_n[which(CH4_Timestep_n<0)]=0

CH4_Timestep_Vecv1=((CH4_Timestep_n)/((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n)))
n_Total_Vec=((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n))

##########################################################

Init_CH4_Perc[g]=CH4_Timestep_n[325]/n_Total_Vec[325]						#New_Percentage_Methane_1yr

##########################################################

#Aerosol_Simulation_Assuming_Aerosols_Only_Produced_in_Pressures_<10Pa			#Coustenis_et_al_2006_Book

Actual_Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)	#Ideal_Gas_Laws

Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)		#Ideal_Gas_Laws
Pressure_Total_Vec[which(Pressure_Total_Vec<10)]=1
Pressure_Total_Vec[which(Pressure_Total_Vec>10)]=0

CH4_Aerosol_Prod2=sum(Photolysis_year[1:325]*Pressure_Total_Vec[1:325])

#####################################

Moles_CH4=CH4_0
n_CH4_Alt_Vec=CH4_Timestep_n
Dens_Air_Surf=((((Moles_CH4[325]*n_Total_Vec[325])/Avog)*RMM_CH4)+(((n_N2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_H2))*1e3			#kg_m^-3

####################################

#Rainfall_assuming_current_20mm_constant_lake_surface_all_methane

Rainfall_Constant=90.62073

Global_Rain_m=(sum(Moles_CH4[299:325]))*Rainfall_Constant
Global_Rain_m3=Global_Rain_m*(0.15*Titan_SA)
Global_Rain_cm3=Global_Rain_m3*1e6
Mass_Global_Rain=Global_Rain_cm3*Dens_CH4
Moles_Global_Rain=Mass_Global_Rain/RMM_CH4									#Moles_of_rain_global_h^-1
Moles_Global_Rain_yr=Moles_Global_Rain*(1e5)								#Moles_of_rain_global_yr^-1
Moles_Global_Rain_cm=Moles_Global_Rain/Vol_Surface_Atm_Layer_cm						#Moles_of_rain_cm^3_h^-1
Moles_CH4_Decrease_cm=Moles_Global_Rain_cm									#Moles_of_rain_cm^3_yr^-1			#Moles_of_rain_cm^3_yr^-1

####################################

Total_n_Vec[n]=data.frame(n_Total_Vec[1:325])
CH4_n_Timestep[n]=data.frame(CH4_Timestep_n[1:325])
CH4_Timestep_Vecv1_n[n]=data.frame(CH4_Timestep_Vecv1[1:325])
F_n[n]=data.frame(F[1:325])
Pressure[n]=data.frame(Pressure_Total_Vec[1:325])
Actual_Pressure[n]=data.frame(Actual_Pressure_Total_Vec[1:325])
Photolysis_Alt[n]=data.frame(Photolysis_year[1:325])
CH4_Aerosol_Prod[n]=CH4_Aerosol_Prod2
Diffusion_Coefficient[n]=data.frame(Diff[1:325])
Diffusion_Actual_Ratio[n]=data.frame(Ratio_Diff[1:325])
Actual_Methane_Increase[n]=data.frame(Increase_in_n_after_1_yr_actual[1:325])
FIR_Altitude[n]=data.frame(FIR[1:325]+FIR_2[1:325]+FIR_3[1:325]+FIR_4[1:325]+FIR_5[1:325]+FIR_6[1:325]+FIR_7[1:325]+FIR_8[1:325])
FIR_Ref_Altitude[n]=data.frame(FIR_Ref[1:325]+FIR_Ref_2[1:325]+FIR_Ref_3[1:325]+FIR_Ref_4[1:325]+FIR_Ref_5[1:325]+FIR_Ref_6[1:325]+FIR_Ref_7[1:325]+FIR_Ref_8[1:325])
FIR_Total_Altitude[n]=data.frame(FIR_Total[1:325])
Temp_Increase_Altitude[n]=data.frame(Temp_Increase[1:325])
Titan_Temperature_Altitude[n]=data.frame(Actual_Titan_Surface_Temp[1:325])
Titan_Surf_Temperature[n]=Actual_Titan_Surface_Temp[325]
Rain[n]=Moles_CH4_Decrease_cm

######################50830_70901######################
}

#############################################################################################################
###################################################LOOP_END##################################################
#############################################################################################################

Total_n_all_2=Total_n_Vec[50830:70901]
Pressure_Alt_2=Pressure[50830:70901]
Actual_Pressure_Alt_2=Actual_Pressure[50830:70901]
CH4_n_all_2=CH4_n_Timestep[50830:70901]
F_n_all_2=F_n[50830:70901]
CH4_Timestep_Vec_2=CH4_Timestep_Vecv1_n[50830:70901]
CH4_Evap_Rate_Vec_2=c(0, E_moles_CH4[50830:70901])
CH4_Perc_Surf_Vec_2=c(0.0567, CH4_Perc_Surf[50830:70901])
Photolysis_Vec_2=Photolysis_Alt[50830:70901]
CH4_Aerosol_Prod_Vec_2=CH4_Aerosol_Prod[50830:70901]
Diff_Coeff_Vec_2=Diffusion_Coefficient[50830:70901]
Diff_Actual_Vec_2=Diffusion_Actual_Ratio[50830:70901]
FIR_Altitude_Vec_2=FIR_Altitude[50830:70901]
FIR_Ref_Altitude_Vec_2=FIR_Ref_Altitude[50830:70901]
FIR_Total_Altitude_Vec_2=FIR_Total_Altitude[50830:70901]
Temp_Increase_Altitude_Vec_2=Temp_Increase_Altitude[50830:70901]
Titan_Temperature_Altitude_Vec_2=Titan_Temperature_Altitude[50830:70901]
Titan_Surface_Temperature_Vec_2=Titan_Surf_Temperature[50830:70901]

#############################################################################################################
#################################################LOOP_TRANSITION#############################################
#############################################################################################################

for (n in 70902:121882)
{

#Infrared

for (y in 2:222)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)

FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=500000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
}

for (y in 223:319)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=200000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

for (y in 320:323)
{	
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=100000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 324:326)
{
FIR_0[y]=FIR[y-1]
FIR_2_0[y]=FIR_2[y-1]
FIR_3_0[y]=FIR_3[y-1]
FIR_4_0[y]=FIR_4[y-1]
FIR_5_0[y]=FIR_5[y-1]
FIR_6_0[y]=FIR_6[y-1]
FIR_7_0[y]=FIR_7[y-1]
FIR_8_0[y]=FIR_8[y-1]
Path_L=50000
FIR[y]=(FIR_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_2[y]=(FIR_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_3[y]=(FIR_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_4[y]=(FIR_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_5[y]=(FIR_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_6[y]=(FIR_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_7[y]=(FIR_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))											#in_cm
FIR_8[y]=(FIR_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))	
}

TotalF=(FIR[326]+FIR_2[326]+FIR_3[326]+FIR_4[326]+FIR_5[326]+FIR_6[326]+FIR_7[326]+FIR_8[326])

#Reflected_IR

F_IRRef_1[n]=B_IR1Ref_Vec[n]*TotalF*0.3
F_IRRef_2[n]=B_IR2Ref_Vec[n]*TotalF*0.3
F_IRRef_3[n]=B_IR3Ref_Vec[n]*TotalF*0.3
F_IRRef_4[n]=B_IR4Ref_Vec[n]*TotalF*0.3
F_IRRef_5[n]=B_IR5Ref_Vec[n]*TotalF*0.3
F_IRRef_6[n]=B_IR6Ref_Vec[n]*TotalF*0.3
F_IRRef_7[n]=B_IR7Ref_Vec[n]*TotalF*0.3
F_IRRef_8[n]=B_IR8Ref_Vec[n]*TotalF*0.3

for (y in 326:323)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]

FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=50000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 322:319)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=100000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 318:223)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=200000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

for (y in 222:1)
{
FIR_Ref_0[y]=FIR_Ref[y]
FIR_Ref_2_0[y]=FIR_Ref_2[y]
FIR_Ref_3_0[y]=FIR_Ref_3[y]
FIR_Ref_4_0[y]=FIR_Ref_4[y]
FIR_Ref_5_0[y]=FIR_Ref_5[y]
FIR_Ref_6_0[y]=FIR_Ref_6[y]
FIR_Ref_7_0[y]=FIR_Ref_7[y]
FIR_Ref_8_0[y]=FIR_Ref_8[y]

Path_L=500000
FIR_Ref[y-1]=(FIR_Ref_0[y]*(exp(-sigma_IR_all[1]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_2[y-1]=(FIR_Ref_2_0[y]*(exp(-sigma_IR_all[2]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_3[y-1]=(FIR_Ref_3_0[y]*(exp(-sigma_IR_all[3]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_4[y-1]=(FIR_Ref_4_0[y]*(exp(-sigma_IR_all[4]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_5[y-1]=(FIR_Ref_5_0[y]*(exp(-sigma_IR_all[5]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_6[y-1]=(FIR_Ref_6_0[y]*(exp(-sigma_IR_all[6]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_7[y-1]=(FIR_Ref_7_0[y]*(exp(-sigma_IR_all[7]*Path_L*n_CH4_Alt_Vec[y-1])))
FIR_Ref_8[y-1]=(FIR_Ref_8_0[y]*(exp(-sigma_IR_all[8]*Path_L*n_CH4_Alt_Vec[y-1])))
}

#Calculating_Absorbed_IR_with_altitude

#Irradiative_IR

for (y in 2:326)
{
FIR[1]=(F_Titan_IR[n]*1e-4)
FIR_2[1]=(F_Titan_IR2[n]*1e-4)
FIR_3[1]=(F_Titan_IR3[n]*1e-4)
FIR_4[1]=(F_Titan_IR4[n]*1e-4)
FIR_5[1]=(F_Titan_IR5[n]*1e-4)
FIR_6[1]=(F_Titan_IR6[n]*1e-4)
FIR_7[1]=(F_Titan_IR7[n]*1e-4)
FIR_8[1]=(F_Titan_IR8[n]*1e-4)
FIR_intervals[y-1]=((FIR[y-1]-FIR[y])+(FIR_2[y-1]-FIR_2[y])+(FIR_3[y-1]-FIR_3[y])+(FIR_4[y-1]-FIR_4[y])+(FIR_5[y-1]-FIR_5[y])+(FIR_6[y-1]-FIR_6[y])+(FIR_7[y-1]-FIR_7[y])+(FIR_8[y-1]-FIR_8[y]))
}
	
#Reflected_IR

for (y in 2:326)
{
FIR_Ref[326]=F_IRRef_1[n]
FIR_Ref_2[326]=F_IRRef_2[n]
FIR_Ref_3[326]=F_IRRef_3[n]
FIR_Ref_4[326]=F_IRRef_4[n]
FIR_Ref_5[326]=F_IRRef_5[n]
FIR_Ref_6[326]=F_IRRef_6[n]
FIR_Ref_7[326]=F_IRRef_7[n]
FIR_Ref_8[326]=F_IRRef_8[n]
FIR_Ref_intervals[y-1]=((FIR_Ref[y]-FIR_Ref[y-1])+(FIR_Ref_2[y]-FIR_Ref_2[y-1])+(FIR_Ref_3[y]-FIR_Ref_3[y-1])+(FIR_Ref_4[y]-FIR_Ref_4[y-1])+(FIR_Ref_5[y]-FIR_Ref_5[y-1])+(FIR_Ref_6[y]-FIR_Ref_6[y-1])+(FIR_Ref_7[y]-FIR_Ref_7[y-1])+(FIR_Ref_8[y]-FIR_Ref_8[y-1]))
}

FIR_Total=FIR_intervals+FIR_Ref_intervals

Temp_Increase=(((FIR_Total*1e4)/s)^(1/4))

###########################################################

#Calculating_Actual_Titan_Surface_Temp

#Assuming_Albedo_30_percent

Actual_Titan_Surface_Temp=((0.7^(1/4))*y_TempTitan_all[n])+Temp_Increase

###########################################################################

#Lyman_Alpha

for (y in 2:221)
{
F[1]=Lyman_Photon_Dens_cm[n]									#Initial_4.55Gyr
F_0[y]=F[y-1]
Path_L=500000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 222:271)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_Particle_Scattering=F[y-1]*(CH4_Aerosol_Prod2/6.539043e-13)
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))
F_N_Diff[y]=(F_0[y])-(F_N[y])			
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_Particle_Scattering+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 272:319)
{
F_0[y]=F[y-1]
Path_L=200000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 320:323)
{	
F_0[y]=F[y-1]
Path_L=100000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

for (y in 324:325)
{
F_0[y]=F[y-1]
Path_L=50000
F_Rayl_N2[y-1]=((CS_N*barn*1e4)*n_N2_Frac_Alt_Vec[y-1]*Path_L)*F_0[y]	
F_N[y]=(F_0[y]*(exp(-(CS_N*barn*1e4)*Path_L*n_N2_Alt_Vec[y-1])))	
F_N_Diff[y]=(F_0[y])-(F_N[y])	
F[y]=(F_0[y]*(exp(-sigma*Path_L*n_CH4_Alt_Vec[y-1])))-(F_Rayl_N2[y-1]+F_N_Diff[y])
F[which(F<0)]=0
}

##########################FACTORING_IN_EVAPORATION_AND_RAINFALL#########################################

#Loop_for_Range_Temp_>110K

A=6.7021														#Antoine_Equation_Constant_A_Prydz_and_Goodwin_1972
B=394.48														#Antoine_Equation_Constant_B_Prydz_and_Goodwin_1972
C=264.609														#Antoine_Equation_Constant_C_Prydz_and_Goodwin_1972

Init_CH4_Perc[n]=Init_CH4_Perc[g]
Methane_CH4[n]=Methane_CH4[g]
Total_Mol_Lake[n]=Total_Mol_Lake[g]
Methane_Mol[n]=Methane_Mol[g]

#Using_Antoine_Equation
pstar_CH4[n]=((10^(A-(B/(C+(Actual_Titan_Surface_Temp[325]-273.15)))))*100)				#SVP_Temp_Range_90K<_T_<110K

CH4_n_surface[n]=CH4_Timestep_n[325]										#per_cm^3
CH4_moles_surface[n]=CH4_n_surface[n]/Avog									#per_cm^3

q_CH4[n]=(0.573*((Methane_CH4[n]*(Init_CH4_Perc[n]*(Actual_Pressure_Total_Vec[325])))/(Actual_Pressure_Total_Vec[325])))			#Specific_Methane_Humidity
qstar_CH4[n]=(0.573*((Methane_CH4[n]*pstar_CH4[n])/(Actual_Pressure_Total_Vec[325])))									#Saturatation_Specific_Humidity_of_Methane_in_Mixture

#To_calculate_Kcoeff

Visc_all[n]=(1.718e-5 + (5.1e-8*(Actual_Titan_Surface_Temp[325] - 273)))
z_0_all[n]=(((Charnock*(Fric_vel^2))/g_Tit)+(0.11*(Visc_all[n]/(Dens_Air_Surf*Fric_vel))))
Kcoeff[n]=(((Karman*Scal_Corr)/(log(z_r/z_0_all[n])))^2)

#To_calculate_CH4_evaporation_rate_kg_m^-2_y^-1

E[n]=Dens_Air_Surf*Kcoeff[n]*(qstar_CH4[n] - q_CH4[n])*Wind_speed						#kg_m^-2_s^-1
E_year[n]=E[n]*3.15576e11												#kg_m^-2_yr^-1

#To_calculate_CH4_evaporation_rate_global_g_yr^-1

E_CH4_global_year[n]=E_year[n]*Lake_SA										#kg_yr^-1
E_CH4_global_year_g[n]=E_CH4_global_year[n]*1000								#g_yr^-1

#To_calculate_evaporation_rate_global_moles_CH4_yr^-1

E_moles_CH4[n]=E_CH4_global_year_g[n]/RMM_CH4									#mol_yr^-1

Increase_in_n_after_1yr[n]=E_moles_CH4[n]*Avog

##########################################################

#To_calculate_new_molar_fraction_CH4_in_lake

Total_Mol_Lake[g]=Total_Mol_Lake[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[g]=Methane_Mol[n]+(Moles_Global_Rain_yr-E_moles_CH4[n])
Methane_Mol[which(Methane_Mol<0)]=0
Methane_CH4[g]=Methane_Mol[g]/Total_Mol_Lake[g]
Methane_CH4[which(Methane_CH4<0)]=0

##############################################ENDING_EVAP##################################################

#Calculating_Diffusion_Methane_Vertical_Transport

Dens_Air=(((Moles_CH4)*RMM_CH4)+(((n_N2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec*n_Total_Vec)/Avog)*RMM_H2))*1e3
zeta_CH4=(Moles_CH4*Avog)/n_Total_Vec
zeta_H2=n_H2_Alt_Vec/n_Total_Vec
zeta_N2=n_N2_Alt_Vec/n_Total_Vec

D_CH4_N2=(A_CH4_N2*(Actual_Titan_Surface_Temp^s_CH4_N2))/Dens_Air
D_CH4_H2=(A_CH4_H2*(Actual_Titan_Surface_Temp^s_CH4_H2))/Dens_Air

Diff[1:325]=(1-zeta_CH4)/(((zeta_N2)/(D_CH4_N2)))

#Troposphere_and_Stratosphere

Ratio_Diff[1:325]=Diff[1:325]/(sum(Diff[1:325]))

Increase_in_n_after_1_yr_actual[299:325]=(Ratio_Diff[299:325]*Increase_in_n_after_1yr[n])/(Vol_Atm_Layer_cm[299:325])
Increase_in_n_after_1_yr_actual[1:298]=(Ratio_Diff[1:298]*Increase_in_n_after_1_yr_actual[299])/(Vol_Atm_Layer_cm[1:298])

############################################################

Moles_CH4[1:325]=Moles_CH4[1:325]+(Increase_in_n_after_1_yr_actual[1:325]/Avog)
Moles_CH4[299:325]=Moles_CH4[299:325]-(Moles_CH4_Decrease_cm)
Moles_CH4[which(Moles_CH4<0)]=0

J_CH4_1=(sigma*phi*F)
Photolysis=(J_CH4_1)*Moles_CH4								#Phot_Rate_per_second
Photolysis_year=(Photolysis*(3.15576e11))							#Phot_Rate_per_100000_years

CH4_0=Moles_CH4-Photolysis_year
CH4_0[which(CH4_0<0)]=0
CH4_Timestep_n=CH4_0*Avog
CH4_Timestep_n[which(CH4_Timestep_n<0)]=0

CH4_Timestep_Vecv1=((CH4_Timestep_n)/((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n)))
n_Total_Vec=((n_Total_Vec)-(n_CH4_Alt_Vec-CH4_Timestep_n))

##########################################################

Init_CH4_Perc[g]=CH4_Timestep_n[325]/n_Total_Vec[325]						#New_Percentage_Methane_1yr

##########################################################

#Aerosol_Simulation_Assuming_Aerosols_Only_Produced_in_Pressures_<10Pa			#Coustenis_et_al_2006_Book

Actual_Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)	#Ideal_Gas_Laws

Pressure_Total_Vec=(n_Total_Vec*K_b*Actual_Titan_Surface_Temp)/(1e-6)		#Ideal_Gas_Laws
Pressure_Total_Vec[which(Pressure_Total_Vec<10)]=1
Pressure_Total_Vec[which(Pressure_Total_Vec>10)]=0

CH4_Aerosol_Prod2=sum(Photolysis_year[1:325]*Pressure_Total_Vec[1:325])

#####################################

Moles_CH4=CH4_0
n_CH4_Alt_Vec=CH4_Timestep_n
Dens_Air_Surf=((((Moles_CH4[325]*n_Total_Vec[325])/Avog)*RMM_CH4)+(((n_N2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_N2)+(((n_H2_Frac_Alt_Vec[325]*n_Total_Vec[325])/Avog)*RMM_H2))*1e3			#kg_m^-3

####################################

#Rainfall_assuming_current_20mm_constant_lake_surface_all_methane

Rainfall_Constant=90.62073

Global_Rain_m=(sum(Moles_CH4[299:325]))*Rainfall_Constant
Global_Rain_m3=Global_Rain_m*(Lake_SA)*0.01
Global_Rain_cm3=Global_Rain_m3*1e6
Mass_Global_Rain=Global_Rain_cm3*Dens_CH4
Moles_Global_Rain=Mass_Global_Rain/RMM_CH4									#Moles_of_rain_global_h^-1
Moles_Global_Rain_yr=Moles_Global_Rain*(1e4)								#Moles_of_rain_global_yr^-1
Moles_Global_Rain_cm=Moles_Global_Rain/Vol_Surface_Atm_Layer_cm						#Moles_of_rain_cm^3_h^-1
Moles_CH4_Decrease_cm=Moles_Global_Rain_cm									#Moles_of_rain_cm^3_yr^-1			#Moles_of_rain_cm^3_yr^-1

####################################

Total_n_Vec[n]=data.frame(n_Total_Vec[1:325])
CH4_n_Timestep[n]=data.frame(CH4_Timestep_n[1:325])
CH4_Timestep_Vecv1_n[n]=data.frame(CH4_Timestep_Vecv1[1:325])
F_n[n]=data.frame(F[1:325])
Pressure[n]=data.frame(Pressure_Total_Vec[1:325])
Actual_Pressure[n]=data.frame(Actual_Pressure_Total_Vec[1:325])
Photolysis_Alt[n]=data.frame(Photolysis_year[1:325])
CH4_Aerosol_Prod[n]=CH4_Aerosol_Prod2
Diffusion_Coefficient[n]=data.frame(Diff[1:325])
Diffusion_Actual_Ratio[n]=data.frame(Ratio_Diff[1:325])
Actual_Methane_Increase[n]=data.frame(Increase_in_n_after_1_yr_actual[1:325])
FIR_Altitude[n]=data.frame(FIR[1:325]+FIR_2[1:325]+FIR_3[1:325]+FIR_4[1:325]+FIR_5[1:325]+FIR_6[1:325]+FIR_7[1:325]+FIR_8[1:325])
FIR_Ref_Altitude[n]=data.frame(FIR_Ref[1:325]+FIR_Ref_2[1:325]+FIR_Ref_3[1:325]+FIR_Ref_4[1:325]+FIR_Ref_5[1:325]+FIR_Ref_6[1:325]+FIR_Ref_7[1:325]+FIR_Ref_8[1:325])
FIR_Total_Altitude[n]=data.frame(FIR_Total[1:325])
Temp_Increase_Altitude[n]=data.frame(Temp_Increase[1:325])
Titan_Temperature_Altitude[n]=data.frame(Actual_Titan_Surface_Temp[1:325])
Titan_Surf_Temperature[n]=Actual_Titan_Surface_Temp[325]
Rain[n]=Moles_CH4_Decrease_cm

######################70902_121882######################
}

#############################################################################################################
###################################################LOOP_END##################################################
#############################################################################################################

Total_n_all_3=Total_n_Vec[70902:121882]
Pressure_Alt_3=Pressure[70902:121882]
Actual_Pressure_Alt_3=Actual_Pressure[70902:121882]
CH4_n_all_3=CH4_n_Timestep[70902:121882]
F_n_all_3=F_n[70902:121882]
CH4_Timestep_Vec_3=CH4_Timestep_Vecv1_n[70902:121882]
CH4_Evap_Rate_Vec_3=c(E_moles_CH4[70902:121882])
CH4_Perc_Surf_Vec_3=c(0.0567, CH4_Perc_Surf[70902:121882])
Photolysis_Vec_3=Photolysis_Alt[70902:121882]
CH4_Aerosol_Prod_Vec_3=CH4_Aerosol_Prod[70902:121882]
Diff_Coeff_Vec_3=Diffusion_Coefficient[70902:121882]
Diff_Actual_Vec_3=Diffusion_Actual_Ratio[70902:121882]
FIR_Altitude_Vec_3=FIR_Altitude[70902:121882]
FIR_Ref_Altitude_Vec_3=FIR_Ref_Altitude[70902:121882]
FIR_Total_Altitude_Vec_3=FIR_Total_Altitude[70902:121882]
Temp_Increase_Altitude_Vec_3=Temp_Increase_Altitude[70902:121882]
Titan_Temperature_Altitude_Vec_3=Titan_Temperature_Altitude[70902:121882]
Titan_Surface_Temperature_Vec_3=Titan_Surf_Temperature[70902:121882]

###################################################

Total_n_all1_2=Total_n_Vec[1:121882]
Pressure_Alt1_2=Pressure[1:121882]
Actual_Pressure_Alt1_2=Actual_Pressure[1:121882]
CH4_n_all1_2=CH4_n_Timestep[1:121882]
F_n_all1_2=F_n[1:121882]
CH4_Timestep_Vec1_2=CH4_Timestep_Vecv1_n[1:121882]
CH4_Evap_Rate_Vec1_2=c(0, E_moles_CH4[1:121882])
CH4_Perc_Surf_Vec1_2=c(0.0567, CH4_Perc_Surf[1:121882])
Photolysis_Vec1_2=Photolysis_Alt[1:121882]
CH4_Aerosol_Prod_Vec1_2=CH4_Aerosol_Prod[1:121882]
Diff_Coeff_Vec1_2=Diffusion_Coefficient[1:121882]
Diff_Actual_Vec1_2=Diffusion_Actual_Ratio[1:121882]
FIR_Altitude_Vec1_2=FIR_Altitude[1:121882]
FIR_Ref_Altitude_Vec1_2=FIR_Ref_Altitude[1:121882]
FIR_Total_Altitude_Vec1_2=FIR_Total_Altitude[1:121882]
Temp_Increase_Altitude_Vec1_2=Temp_Increase_Altitude[1:121882]
Titan_Temperature_Altitude_Vec1_2=Titan_Temperature_Altitude[1:121882]
Titan_Surface_Temperature_Vec1_2=Titan_Surf_Temperature[1:121882]

####################################################################################################################################################################################################################################################################
####################################################################################################################################################################################################################################################################