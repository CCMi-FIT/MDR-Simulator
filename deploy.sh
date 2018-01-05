#!/bin/bash

D="~/rsync/darci/"

rsync -rvpLz --delete -e ssh web/* rob@ccmi.fit.cvut.cz:$D
#ssh -t rob@ccmi.fit.cvut.cz "sudo -u www-data cp -r $D/* /var/www/wordpress/darci/"

