[GPF Home Page](http://www.urbanjost.altervista.org/LIBRARY/libGPF/GPF.html)

# KRACKEN(3F): The Fortran Command Line Argument Cracker  
*(Extended Version)*

## Supports FPM ![fpm](docs/images/fpm_logo.gif)
This is an fpm(1) package and not a stand-alone module. It requires fpm(1)
to build.

   download the github repository and build it with 
   fpm ( as described at [Fortran Package Manager](https://github.com/fortran-lang/fpm) )
   
   ```bash
        git clone https://github.com/urbanjost/M_kracken.git
        cd M_kracken
        fpm build
        fpm test
   ```
   
   or just list it as a dependency in your fpm.toml project file.
   
```toml
        [dependencies]
        M_kracken = { git = "https://github.com/urbanjost/M_kracken.git" }
```
## DOCUMENTATION
  - Procedure [Documentation](https://urbanjost.github.io/M_kracken)
  - [Index](man3.html)
