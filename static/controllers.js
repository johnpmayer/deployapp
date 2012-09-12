
var snap_form_headers = 
    { 'Content-Type' : 'application/x-www-form-urlencoded'}

var copout = function() {
    alert("Something went wrong, contact your edge-case engineer");
}

// For use across tabs
function Deploy($scope, $http) {
    
    $scope.disk_lookup = {}
    
    $scope.reload_disks = function() {
        $http.get('app/disks')
            .success(function(data) {
                $scope.disks = data;
                
                //alert("Disks: " + JSON.stringify($scope.disks));
                
                for (var index in $scope.disks) {
                    var disk = $scope.disks[index];
                    //alert("Disk: " + JSON.stringify(disk));
                    $scope.disk_lookup[disk.name] = disk.id;
                }
                
                //alert("Lookup: " + 
                //JSON.stringify($scope.disk_lookup));
                
            }).error(copout);
    }
    
    $scope.profile_lookup = {}
    
    $scope.reload_profiles = function() {
        $http.get('app/profiles')
            .success(function(data) {
                $scope.profiles = data;
                
                for (var index in $scope.profiles) {
                    
                    var profile = $scope.profiles[index];
                    $scope.profile_lookup[profile.name] = profile.id;

                }
                
            }).error(copout);
    }
    
    
}

function Hosts($scope, $http) {
    
    $http.get('app/hosts').success(function(data) {
        $scope.hosts = data;
    });
    
    $scope.hostOrderProp = "hw_address";
}

function Host($scope, $http) {
    
    $scope.update_host_profile = function() {
        
        alert('Changing host ' + $scope.host.hw_address +
              ' to profile:  ' + $scope.host.new_profile.id +
              ':' + $scope.host.new_profile.name + '!');
        
        
    }

}

function Profiles($scope, $http) {
        
    $scope.reload_profiles();
    
    $scope.new_profile = {}
    
    $scope.create_profile = function() {
        
        var disk_id = 
            $scope.disk_lookup[$scope.new_profile.disk.name];
        
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

function New_Partition($scope, $http) {

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
