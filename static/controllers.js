
var snap_form_headers = 
    { 'Content-Type' : 'application/x-www-form-urlencoded'}

var copout = function() {
    alert("Something went wrong, contact your edge-case engineer");
}

// For use across tabs
function Deploy($scope, $http) {
    
    $scope.disk_lookup = {}
    $scope.disk_reverse_lookup = {}
    
    $scope.reload_disks = function() {
        $http.get('app/disks')
            .success(function(data) {
                $scope.disks = data;
                
                //alert("Disks: " + JSON.stringify($scope.disks));
                
                for (var index in $scope.disks) {
                    var disk = $scope.disks[index];
                    //alert("Disk: " + JSON.stringify(disk));
                    $scope.disk_lookup[disk.id] = disk.name;
                    $scope.disk_reverse_lookup[disk.name] = disk.id;
                }
                
                //alert("Lookup: " + 
                //JSON.stringify($scope.disk_lookup));
                
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
    
    $scope.reload_ips();
    
}

function Hosts($scope, $http) {
    
    $scope.reload_hosts = function() {
        $http.get('app/hosts').success(function(data) {
            $scope.hosts = data;
        });
    }
    
    $scope.reload_hosts();
    
    $scope.hostOrderProp = "hw_address";
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
        
        alert('changing host ip to ' + $scope.show_ip($scope.host.new_ip));
        
        var host_id = $scope.host.id;
        var ip_address = $scope.host.new_ip;
        
        var data = $.param({ host_id    : host_id,
                             ip_address : ip_address });
        
        $http({ method         : 'POST',
                url            : 'app/host/ip',
                data           : data,
                headers        : snap_form_headers }
             ).success(function(data) {
                 alert(data);
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
        
        alert('pinging host ip ' + $scope.show_ip($scope.host.ip_address))
        
        $scope.host.ping.in_progress = true;

        var host_id = $scope.host.id; // ToDo currently unused by app
        var ip_address = $scope.host.ip_address;
        
        var data = $.param({ host_id    : host_id,
                             ip_address : ip_address });
        
        $http({ method         : 'POST',
                url            : 'app/host/ping',
                data           : data,
                headers        : snap_form_headers }
             ).success(function(data) {
                 alert(data);
                 $scope.host.ping.in_progress = false;
                 var resp = data;
                 if (resp == 0) {
                     $scope.host.ping.response = "OK";
                 } else {
                     $scope.host.ping.response = "Failed";
                 }
             }).error(copout);
        
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

    
}

function Profiles($scope, $http) {
        
    $scope.reload_profiles();
    
    $scope.new_profile = {}
    
    $scope.create_profile = function() {
        
        var disk_id = 
            $scope.disk_reverse_lookup[$scope.new_profile.disk.name];
        
        var data = 
            $.param({ name : $scope.new_profile.name,
                      description : $scope.new_profile.description,
                      disk_id : disk_id });
        
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
    
}

function Disks($scope, $http) {
    
    $scope.reload_disks();
    
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
        $http.get('app/partitions/'+$scope.disk.id)
            .success(function(data) {
                $scope.partitions = data;
            }).error(copout);
    };
    
    $scope.reload_partitions();
    
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
        
        $http({ method    : 'PUT',
                url       : 'app/partition',
                data      : $.param(
                    { disk_id          : $scope.disk.id,
                      partition_number : $scope.new_partition.number,
                      partition_type   : $scope.new_partition.type,
                      size_in_mb       : $scope.new_partition.size }
                ),
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
                url       : 'app/partition',
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

