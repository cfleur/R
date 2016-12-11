##################################################################
# Approximating the safe thickness of Ice to shelter humans from #
#                    cosmic radiation on Mars                    #
##################################################################

# Note that the source used in this experiment has only one energy,
# whereas in space there are many energies of radiation. Absorption
# in radiation by shielding material depends on energy of radiation.
# The energies used in this experiment were 1173.2 and 1332.5 keV
# gammay rays, emmited from a Co-60 source.


ice <- read.table("IceThickness_NumofPulses.txt", header=T)
water <- read.table("WaterThickness_NumofPulses.txt", header=T)
# This imports the data.

# Lets just check the data...
water
ice

# The gamma radiation is measured by counting the number of pulses
# in 60 seconds. 

# Now the errors are needed: 
error_water <- read.table("Error_WaterNumofPulses.txt", header=T)
error_ice <- read.table("Error_IceNumofPulses.txt", header=T)
# Append these tables: (maybe not necessary but why not)
errors_ice_water <- cbind(error_ice,error_water)

##################################################################
#                    Calculations and Plotting                   #
##################################################################

# Now since the equation of radioactive decay is exponential,
# i.e. N = N_0^-mu*X, where N is the number of pulses counted,
# N_0 is the number of pulses emmited, mu is the attenuation 
# constant (cm^-1), and X is the thickness of the shield.
# By making this equation linear, we see that mu is the slope
# of a straight line: ln(N) = ln(N_0) + mu*X and the pulses 
# emmited is the Y-intercept.

ln_ice <- cbind(log(ice$Number_of_pulses_ice_gm60), ice$Thickness_ice)
# Now number of pulses is in a logarithmic scale

fit.ln_ice <- lm(log(Number_of_pulses_ice_gm60) ~ Thickness_ice, data=ice)
# This creates a linear model w.r.t to ln_ice
# Note that ice data was used because ln_ice is not in the correct
# format to be used as a data frame (?)

# OK, now lets do this for water.

ln_water <- cbind(log(water$Number_of_pulses_water_gm60), water$Thickness_water)
# Now number of pulses is in a logarithmic scale

fit.ln_water <- lm(log(Number_of_pulses_water_gm60) ~ Thickness_water, data=water)
# This creates a linear model w.r.t to ln_ice
# Note that ice data was used because ln_ice is not in the correct
# format to be used as a data frame (?) need some clarification

################################ Plotting #####################################
# Lets plot this:
plot(ln_ice[,2],ln_ice[,1], col="cyan4", pch=16, cex=.9,
     main="Title",
     xlab="Thickness of shielding material (mm)",
     ylab="Natural log number of pulses",
     xlim=c((min(ln_water[,2],ln_ice[,2])),(max(ln_water[,2],ln_ice[,2]))), 
     ylim=c((min(ln_water[,1],ln_ice[,1])),(max(ln_water[,1],ln_ice[,1]))) 
    )
# Had to rearrange dependent and independent variable in columns of ln_ice
     
# Lets add the linear model (could also use lines())
points(x=ln_ice[,2], y=fit.ln_ice$fitted.values, type="l", col="cyan4", lwd=2)

# Lets add water to the plot
points(x=ln_water[,2], y=ln_water[,1], col="cornflowerblue", pch=16, cex=.9)
points(x=ln_water[,2], y=fit.ln_water$fitted.values, col="cornflowerblue", type="l", lwd=2)


############################## See the summaries ###############################
# Ice:
cat("###################################################
    \nLinear model summary of ice:
    \n###################################################\n")
summary(fit.ln_ice)
cat("###################################################
    \nAgain, the coefficinets are (we need these in a second):
    \n###################################################\n")
coefficients(fit.ln_ice)
# And water
cat("###################################################
    \nLinear model summary of water:
    \n###################################################\n") 
print(summary(fit.ln_water))
cat("###################################################
    \nAgain, the coefficinets are (we need these in a second):
    \n###################################################\n")
coefficients(fit.ln_water)

# Cat for fun ~^.^~ --- nothing is printed on source yet except the cats!

############################ Half value thickness ############################

# The quantity knows as half value thickenss is found when the attenuation has 
# reached half of the original emission. This is found by finding half of N_0.
# Since we are using a logarithmic scale, we need to calculate ln(0.5N_0). 
# Lets do this for ice and water.

ice_coefs <- coef(fit.ln_ice)
water_coefs <- coef(fit.ln_water)
# Now ice_coefs and water_coefs has intercept and slope of both models.

# Remember that the intercept is equal to N_0
# We need to take the log of half the original value of N_0
# This is numerically equivalent in absolute value as ln(2)/mu.
half_ice_pulses <- log(0.5*exp(ice_coefs[1]))
half_water_pulses <- log(0.5*exp(water_coefs[1]))

cat("The half value of pulses for ice is:",
    half_ice_pulses) 
cat("\nand for water is:",
    half_water_pulses)

# Now we need to know the thickenss at the half pulse value
# We rearrange the linear equation to solve for x: x = (y-b)/k, where k is the 
# slope any b is the y intercept.
# Lets rename our variables to make this easier to comprehend.
ice_slope <- ice_coefs[2]
ice_intercept <- ice_coefs[1]
water_slope <- water_coefs[2]
water_intercept <- water_coefs[1]

half_ice_thickness <- (half_ice_pulses - ice_intercept)/ice_slope
half_water_thickness <- (half_water_pulses - water_intercept)/water_slope

# Lets add these to the plot. The plot will need to be enlarged, so lets remake it.

plot(ln_ice[,2],ln_ice[,1], col="cyan4", pch=16, cex=.9,
     main="Attenuation of gamma-decay in water and ice",
     xlab="Thickness of shielding material (mm)",
     ylab="Natural log number of pulses",
     xlim=c((min(ln_water[,2],ln_ice[,2],half_ice_thickness,half_water_thickness)),(max(ln_water[,2],ln_ice[,2],half_ice_thickness,half_water_thickness))+5), 
     ylim=c((min(ln_water[,1],ln_ice[,1],half_ice_pulses,half_water_pulses)),(max(ln_water[,1],ln_ice[,1],half_ice_pulses,half_water_pulses))) 
)

# Had to rearrange dependent and independent variable in columns of ln_ice

# Lets add the linear model (could also use lines())
points(x=ln_ice[,2], y=fit.ln_ice$fitted.values, type="l", col="cyan4", lwd=2)

# Lets add water to the plot
points(x=ln_water[,2], y=ln_water[,1], col="cornflowerblue", pch=16, cex=.9)
points(x=ln_water[,2], y=fit.ln_water$fitted.values, col="cornflowerblue", type="l", lwd=2)

# And the half thickness values:

points(x=half_ice_thickness, y=half_ice_pulses, col="orange", pch=16, cex=.9)
text(half_ice_thickness, half_ice_pulses, 
     "Half value\nlayer ice\n149.3 mm",
     pos=3, col="orange", cex=.6)
points(x=half_water_thickness, y=half_water_pulses, col="red", pch=16, cex=.9)
text(half_water_thickness, half_water_pulses, 
     "Half value\nlayer water\n105.4 mm",
     pos=3, col="red", cex=.6)
# (?) Don't know how to add the x value to graph point as a variable instead of as a string. 

legend(120,5.1, c("Ice", "Water"), col = c("cyan4", "cornflowerblue"), pch=c(16,16), cex=.9)

# The slope of the linear model is equal to the attenuation constant, mu (cm^-1)
mu_ice <- ice_coefs[2]
mu_water <- water_coefs[2]
# Lets ad this to the graph
text(20, 3.8, 
     "mu ice = -0.004642953",
      col="cyan4", cex=.6)
text(40, 4.55, 
     "mu water = -0.00657598",
      col="cornflowerblue", cex=.6)

# Add minor tick marks
library(Hmisc)
minor.tick(nx=4, ny=2, tick.ratio=.5)
grid()

# Add standard error bars
w_stderr <- read.table("Error_WaterNumofPulses.txt", header = T)
ln.w_stderr <- log(w_stderr)
yw <- ln_water[,1]
xw <- ln_water[,2]
w_stderr_plus <- yw + ln.w_stderr
w_stderr_minus <- yw - ln.w_stderr
arrows(y0=yw, x0=xw-1, y1=yw, x1=xw+1, length=0.05, angle=90, code=3, col="cornflowerblue")
arrows(x0=xw, y0=w_stderr_minus, x1=xw, y1=w_stderr_plus, length=0.05, angle=90, code=3, col="cornflowerblue")

i_stderr <- read.table("Error_IceNumofPulses.txt", header = T)
ln.i_stderr <- log(i_stderr)
yi <- ln_ice[,1]
xi <- ln_ice[,2]
i_stderr_plus <- yi + ln.i_stderr
i_stderr_minus <- yi - ln.i_stderr
arrows(y0=yi, x0=xi-0.3, y1=yi, x1=xi+0.3, length=0.05, angle=90, code=3, col="cyan4")
arrows(x0=xw, y0=w_stderr_minus, x1=xw, y1=w_stderr_plus, length=0.05, angle=90, code=3, col="cornflowerblue")

#arrows(xw[sw], yw[sw]-w_stderr[sw], xw[sw], yw[sw]+w_stderr[sw], length=0.05, angle=90, code=3)
#arrows(ln_water[,1], ln_water[,2]-1, ln_water[,1], ln_water[,1]+1, length=0.05, angle=90, code=3)

#(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)



