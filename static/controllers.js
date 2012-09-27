
var snap_form_headers = 
    { 'Content-Type' : 'application/x-www-form-urlencoded'}

var copout = function(data) {
    alert("Something went wrong, contact your edge-case engineer:\n " + data);
}
 
// For use across tabs
function Deploy($scope, $http) {
    
    $scope.logged_in = {}
    $scope.logged_in.status = false;

    $scope.set_logged_in = function(bool) {
        //alert("in deploy set_logged_in")
        //alert(bool)
        $scope.logged_in.status = bool;
    };
    
    $scope.loading = 0;
    
    $scope.copout = function(data) {
        $scope.loading -= 1;
        alert("Something went wrong, contact your edge-case engineer:\n " + data);
    }
    
    $scope.disk_lookup = {}
    
    $scope.reload_disks = function() {
        $scope.loading += 1;
        $http.get('app/disks')
            .success(function(data) { 
                $scope.disks = data;
                
                //alert("Disks: " + JSON.stringify($scope.disks));
                
                for (var index in $scope.disks) {
                    var disk = $scope.disks[index];
                    //alert("Disk: " + JSON.stringify(disk));
                    $scope.disk_lookup[disk.id] = disk.name;
                }
                
                //alert("Lookup: " + 
                //JSON.stringify($scope.disk_lookup));
                $scope.loading -= 1;
            }).error(copout);
    }
    
    // Load the images
    $scope.image_lookup = {}
    $scope.reload_images = function() {
        $http.get('app/images')
            .success(function(data) { 
                $scope.images = data;
                for (var index in $scope.images) {
                    var image = $scope.images[index];
                    $scope.image_lookup[image.id] = image.name; 
                    // ToDo ref the entire image
                }
            }).error(copout);
    }
    
    $scope.profile_lookup = {}
    
    $scope.reload_profiles = function() {
        
        $scope.profile_lookup[-1] = 'unassigned';
        
        $http.get('app/profiles')
            .success(function(data) { 
                $scope.profiles = data;
                
                for (var index in $scope.profiles) {
                    
                    var profile = $scope.profiles[index];
                    $scope.profile_lookup[profile.id] = profile.name;
                    
                }
                
            }).error(copout);
    }
        
    $scope.reload_ips = function() {
        $scope.ips = [];
        for (var i = 3; i <= 99; i += 1) {
            $scope.ips.push(i)
        }
    }
    
    $scope.reload_softwares = function() {
        $scope.loading += 1;
        $http.get('app/softwares')
            .success(function(data) { 
                $scope.softwares = data;
                $scope.loading -= 1;
            })
            .error($scope.copout);
    };
    
    $scope.reload_hosts = function() {
        $http.get('app/hosts').success(function(data) { 
            $scope.hosts = data;
        });
    }
    
    $scope.reload_images = function() {
        $http.get('app/images/')
            .success(function(data) { 
                $scope.images = data;
            }).error(copout);
    };
    
    $scope.reload_top_level = function() {
        $scope.reload_hosts();
        $scope.reload_profiles();
        $scope.reload_images();
        $scope.reload_ips();
        $scope.reload_softwares();
        $scope.reload_disks();
    }
    
}

function Login($scope, $http) {
    
    $http({ method   : 'GET',
            url      : 'app/check'}
         ).success(function(data) { 
             if (JSON.parse(data) === true) {
                 $scope.set_logged_in(true);
                 $scope.reload_top_level();
             }
             
         }).error(copout);
        
    $scope.login = function() {
        var data = $.param({ username : $scope.username,
                             password : $scope.password })
                           
        //alert(JSON.stringify(data))
        
        $http({ method    : 'POST',
                url       : 'app/login',
                data      : data,
                headers   : snap_form_headers}
             ).success(function(data) { 
                 //alert(data);
                 if (JSON.parse(data) === true) {
                     $scope.set_logged_in(true);
                 }
                 //alert($scope.logged_in.status)
                 $scope.reload_top_level();
             }).error(copout);
        
    }
}

function Hosts($scope, $http) {
    
    
    $scope.hostOrderProp = "hw_address";
    
    $scope.deploy_stage_lookup = ["Not staged","Staged","Hot"];
}

function Host($scope, $http) {

    $scope.update_host_profile = function() {
        
        //alert('Changing host (id=' + $scope.host.id + '):' +
        //$scope.host.hw_address +
        //' to profile:  ' + $scope.host.new_profile.id +
        //':' + $scope.host.new_profile.name + '!');
        
        var host_id = $scope.host.id;
        var profile_id = $scope.host.new_profile.id;
        
        var data = $.param({ host_id    : host_id,
                             profile_id : profile_id });

        $http({ method        : 'POST',
                url           : 'app/host/profile',
                data          : data,
                headers       : snap_form_headers }
              ).success(function(data) { 
                  //alert(data);
                  $scope.reload_hosts()
              }).error(copout);
        
    }
    
    $scope.update_host_ip = function() {
        
        //alert('changing host ip to ' + $scope.show_ip($scope.host.new_ip));
        
        var host_id = $scope.host.id;
        var ip_address = $scope.host.new_ip;
        
        var data = $.param({ host_id    : host_id,
                             ip_address : ip_address });
        
        $http({ method         : 'POST',
                url            : 'app/host/ip',
                data           : data,
                headers        : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_hosts();
             }).error(copout);
        
    }
    
    $scope.show_ip = function(subnet_ip) {

        if (subnet_ip > 0) {
            return '192.168.1.' + subnet_ip;
        } 
        
        return 'unassigned';

    }
    
    $scope.host.ping = { in_progress : false,
                         response    : undefined }
    
    $scope.ping_host = function() {
        
        //alert('pinging host ip ' + $scope.show_ip($scope.host.last_reported_ip))
        
        $scope.host.ping.in_progress = true;

        var host_id = $scope.host.id; // ToDo currently unused by app
        var ip_address = $scope.host.last_reported_ip;
        
        var data = $.param({ host_id    : host_id,
                             ip_address : ip_address });
        
        $http({ method         : 'POST',
                url            : 'app/host/ping',
                data           : data,
                headers        : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 $scope.host.ping.in_progress = false;
                 var resp = data;
                 if (resp == 0) {
                     $scope.host.ping.response = "OK";
                     $scope.host.reboot.in_progress = false;
                 } else {
                     $scope.host.ping.response = "Failed";
                 }
             }).error(function() {
                 $scope.host.ping.in_progress = false;
                 copout()
             });
        
    }
    
    //$scope.ping_host(); this started to get ridiculous
    
    $scope.host.reboot = { in_progress : false }
    
    $scope.reboot_host = function() {
        
//        $scope.host.reboot.in_progress = true;

        var host_id = $scope.host.id; // ToDo currently unused by app
        var ip_address = $scope.host.last_reported_ip;
        
        var data = $.param({ host_id    : host_id,
                             ip_address : ip_address });
        
        $http({ method         : 'POST',
                url            : 'app/host/reboot',
                data           : data,
                headers        : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 var resp = data;
                 if (resp == 0) {
                     $scope.host.reboot.in_progress = true;
                 }
             }).error(function() {
                 copout()
             });
        
    }
    
    $scope.unassign_host_profile = function() {
        
        //alert('Unassigning host (id=' + $scope.host.id + '):' +
        //$scope.host.hw_address +
        //' profile:  ');
        
        var host_id = $scope.host.id;
        
        var data = $.param({ host_id    : host_id });

        $http({ method        : 'DELETE',
                url           : 'app/host/profile',
                data          : data,
                headers       : snap_form_headers }
              ).success(function(data) { 
                  //alert(data);
                  $scope.reload_hosts()
              }).error(copout);
        
    }
    
    $scope.unassign_host_ip = function() {
        
        //alert('Unassigning host ip (id=' + $scope.host.id + ')');
        
        var host_id = $scope.host.id;
        
        var data = $.param({ host_id    : host_id });

        $http({ method        : 'DELETE',
                url           : 'app/host/ip',
                data          : data,
                headers       : snap_form_headers }
              ).success(function(data) { 
                  //alert(data);
                  $scope.reload_hosts()
              }).error(copout);
        
    }

    $scope.stage_host = function() {
        var host_id = $scope.host.id;
        var data = $.param({host_id:host_id});
        $http({ method  : 'POST',
                url     : 'app/host/stage',
                data    : data,
                headers : snap_form_headers }
             ).success(function(data) { 
                 alert(data);
                 $scope.reload_hosts();
             }).error(copout);
    }
    
}

function Profiles($scope, $http) {
        
    
    $scope.new_profile = {}
    
    $scope.create_profile = function() {
        
        var data = 
            $.param({ name : $scope.new_profile.name,
                      description : $scope.new_profile.description,
                      disk_id : $scope.new_profile.disk.id,
                      image_id : $scope.new_profile.image.id });
        
        $http({ method       : 'PUT',
                url          : 'app/profile',
                data         : data,
                headers      : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_profiles();
             }).error(copout);
        
    }
                  
    /*
      $scope.test_lookup = function(disk_name) {
      //alert("Disks: " + JSON.stringify($scope.disks));
      //alert("Selection: " + JSON.stringify(disk_name));
      //alert($scope.disk_lookup);
      alert("Disk id: " + $scope.disk_lookup[disk_name]);
      }
    */
}

function Profile($scope, $http) {
    
    $scope.delete_profile = function() {
        
        //alert('Attempting to delete profile ' +
        //$scope.profile.id + ':' + 
        //$scope.profile.name + "!");
        
        var id = $scope.profile.id;
        var data = $.param({ profile_id: id });
        
        $http({ method     : 'DELETE',
                url        : 'app/profile',
                data       : data,
                headers    : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_profiles();
             }).error(copout);
        
    }
    
    $scope.profile.reload_packages = function() {
        
        // Todo GET app/profile/packages
        var profile_id = $scope.profile.id;
        
        $http({ method : 'GET',
                url    : 'app/profile/' + profile_id + '/packages' }
             ).success(function(data) { 
                 $scope.profile.packages = data;
             }).error(copout);
        
    }
    
    if($scope.logged_in.status) {
        $scope.profile.reload_packages();
    }
    
}

function NewSoftware($scope, $http) {
    
    $scope.add_package = function() {
        var package_id = $scope.software.id;
        var profile_id = $scope.profile.id;
        alert("Adding " + package_id + " to profile " + profile_id + ".");
        
        var data = $.param({ package_id : package_id,
                             profile_id : profile_id });
        
        $http({ method  : 'PUT',
                url     : 'app/profile/package',
                data    : data,
                headers : snap_form_headers
              }
             ).success(function(data) { 
                 alert(data);
                 $scope.profile.reload_packages();
             }).error(copout);
    }
    
    
}

function ExistingSoftware($scope, $http) {
    
    // ToDo: DELETE app/profile/package
    $scope.remove_package = function() {
        var package_id = $scope.software.id;
        var profile_id = $scope.profile.id;
        alert("Removing " + package_id + " from profile " + profile_id + ".");
        
        var data = $.param({ package_id : package_id,
                             profile_id : profile_id });
        
        $http({ method  : 'DELETE',
                url     : 'app/profile/package',
                data    : data,
                headers : snap_form_headers
              }
             ).success(function(data) { 
                 alert(data);
                 $scope.profile.reload_packages();
             }).error(copout);
    }
    
}

function Images($scope, $http) {
    
}

function Disks($scope, $http) {
    
    
    $scope.new_disk = {}
    
    $scope.active_details = []
    $scope.current_active = undefined
    
    $scope.activate = function(disk_id) {
        if ($scope.current_active !== undefined) {
            $scope.active_details[$scope.current_active] = false;
        }
        $scope.current_active = disk_id;
        $scope.active_details[disk_id] = true;
    }
    
    $scope.create_disk = function() {
        
        //        alert("Creating new disk layout: '"
        //             + $scope.new_disk.name
        //            + "'.");

        $http({method  : 'PUT',
               url     : 'app/disk',
               data    : $.param({ disk_name : 
                                   $scope.new_disk.name }),
               headers : snap_form_headers}
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_disks();
             }).error(copout);
        
    }
    
}

function Disk($scope, $http) {
    
    $scope.delete_disk = function() {
        
        //alert("Deleting disk " 
        //    + $scope.disk.disk_id + ":" + 
        //  $scope.disk.disk_name + "!")
        
        $http({method  : 'DELETE',
               url     : 'app/disk',
               data    : $.param({ disk_id :
                                   $scope.disk.id }),
               headers : snap_form_headers}
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_disks();
             }).error(copout);
        
    }
    
}

function Partitions($scope, $http) {
    
    $scope.reload_partitions = function() {
        $http.get('app/disk/'+$scope.disk.id+"/partitions")
            .success(function(data) { 
                $scope.partitions = data;
            }).error(copout);
    };
    
    if ($scope.logged_in.status) {
        $scope.reload_partitions();
    }
    
    $scope.active_details[$scope.disk.id] = false;
    
    $scope.types = [ "Primary", "Extended", "Logical" ];
    
    $scope.order_prop = "number";
    
    $scope.calc_type_id = function() {
        if ($scope.new_partition.type === "Extended") {
            return 0;
        } else {
            return $scope.new_partition.number;
        }
    };
    
    $scope.new_partition = {}
    
    $scope.add_partition = function() {
        //alert($scope.new_partition.number
        //      + ';' + $scope.calc_type_id()
        //    + ';' + $scope.new_partition.size);
        
        var data = $.param(
            { disk_id          : $scope.disk.id,
              partition_number : $scope.new_partition.number,
              partition_type   : $scope.new_partition.type,
              mount_point      : $scope.new_partition.mount,
              is_boot          : ($scope.new_partition.is_boot ? 1 : 0),
              size_in_mb       : $scope.new_partition.size 
            }
        );
        
        $http({ method    : 'PUT',
                url       : 'app/disk/partition',
                data      : data,
                headers   : snap_form_headers}
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_partitions();
             }).error(copout);
        
    }
    
}

function Partition($scope, $http) {
    
    $scope.from_type_id = function(type_id) {
        if (type_id === 0) {
            return "Extended";
        } else if (type_id <= 4) {
            return "Primary"
        } else {
            return "Logical"
        }
    }
    
    $scope.delete_partition = function() {
        /*
          alert('Attempting to delete partition id"'
          + $scope.partition.partition_id + '!');
        */
        
        $http({ method    : 'DELETE',
                url       : 'app/disk/partition',
                data      : $.param(
                    { partition_id: $scope.partition.id }
                ),
                headers : snap_form_headers }
             ).success(function(data) { 
                 //alert(data);
                 $scope.reload_partitions();
             }).error(copout);
        
    }
    
}

