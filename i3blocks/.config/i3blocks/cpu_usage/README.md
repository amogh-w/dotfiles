# cpu_usage

Show CPU usage.

![](cpu_usage.png)

# Dependencies

* `mpstat`

# Config

```
[cpu_usage]
command=$SCRIPT_DIR/cpu_usage
interval=10
LABEL=CPU 
#min_width=CPU: 100.00%
#T_WARN=50
#T_CRIT=80
#DECIMALS=2
```
