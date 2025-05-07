## Info - autovi.web 0.1.1

This is a shiny web application of the [`autovi`](https://github.com/TengMCing/autovi) R package. 

This application helps you automatically detect potential violations in your regression model by analyzing residual plots using **computer vision and statistical inference**.  

### How It Works
1. **Upload** your CSV file containing residuals and fitted values.  
2. The model computes a **Visual Signal Strength (VSS)**, quantifying deviations from ideal patterns.  
3. **Null Comparison:**  
   - Your plot's VSS is compared against a distribution of VSS values from **simulated null plots** (correctly specified models).  
   - A **p-value** is calculated to determine if the deviation is statistically significant.  
4. **Bootstrap Validation:**  
   - Your data is resampled to generate multiple bootstrap replicates.  
   - We check what percentage of these replicates **also show significant violations** (p < 0.05) when analyzed the same way.  
   - This tells you whether the original decision (significant/non-significant) is **robust** to sampling variability.  
5. **Gradient Attention Map:** Highlights which regions of your plot most influence the VSS, helping you interpret the patterns.  

### Learn More

Interested in trying automated visual inference locally? Explore our R package [autovi](https://github.com/TengMCing/autovi).

Want to learn more about the methodology? Read our [paper](https://patrick-li-thesis.netlify.app/03-chap3).

### Contact

Author: Weihao (Patrick) Li

- <i class="fab fa-github" role="presentation" aria-label="github icon"></i> [TengMCing](https://github.com/TengMCing)
- <i class="far fa-envelope" role="presentation" aria-label="envelope icon"></i> [patrick.li@anu.edu.au](mailto:patrick.li@anu.edu.au)
- <i class="fas fa-location-dot" role="presentation" aria-label="location-dot icon"></i> 46 Sullivans Creek Rd, Acton ACT 2601, Australia  

Co-authors:
- Di Cook (dicook@monash.edu)
- Emi Tanaka (emi.tanaka@anu.edu.au)
- Susan VanderPlas (susan.vanderplas@unl.edu)
- Klaus Ackermann (Klaus.Ackermann@monash.edu) 


