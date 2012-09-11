
var snap_form_headers = 
    { 'Content-Type' : 'application/x-www-form-urlencoded'}

function Hosts($scope, $http) {
    
    $http.get('app/hosts').success(function(data) {
        $scope.hosts = data;
    });
    
    //$scope.orderProp = "host_id";
}

function Profiles($scope, $http) {
    
    $http.get('app/profiles').success(function(data) {
        $scope.profiles = data;
    });
    
}

function Disks($scope, $http) {
    
    $scope.reload_disks = function() {
        $http.get('app/disks').success(function(data) {
            $scope.disks = data;
        });
    }
    
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
             });
        
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
                                   $scope.disk.disk_id }),
               headers : snap_form_headers}
             ).success(function(data) {
                 //alert(data);
                 $scope.reload_disks();
             });
        
    }
    
}

function Partitions($scope, $http) {
    
    $scope.reload_partitions = function() {
        $http.get('app/partitions/'+$scope.disk.disk_id)
            .success(function(data) {
                $scope.partitions = data;
            });
    };
    
    $scope.reload_partitions();
    
    $scope.active_details[$scope.disk.disk_id] = false;
    
    $scope.types = [ "Primary", "Extended", "Logical" ];
    
    $scope.order_prop = "partition_number";
    
}

function Partition($scope, $http) {
    
    /*
    $scope.from_type_id = function(type_id) {
        if (type_id === 0) {
            return "Extended";
        } else if (type_id <= 4) {
            return "Primary"
        } else {
            return "Logical"
        }
    }
    */
    
    $scope.delete_partition = function() {
        /*
          alert('Attempting to delete partition id"'
          + $scope.partition.partition_id + '!');
        */
        
        $http({ method    : 'DELETE',
                url       : 'app/partition',
                data      : $.param(
                    { partition_id: $scope.partition.partition_id }
                ),
                headers : snap_form_headers }
             ).success(function(data) {
                 //alert(data);
                 $scope.reload_partitions();
             });
        
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
                    { disk_id          : $scope.disk.disk_id,
                      partition_number : $scope.new_partition.number,
                      partition_type   : $scope.new_partition.type,
                      size_in_mb       : $scope.new_partition.size }
                ),
                headers   : snap_form_headers}
             ).success(function(data) {
                 //alert(data);
                 $scope.reload_partitions();
             });
        
    }
    
}
