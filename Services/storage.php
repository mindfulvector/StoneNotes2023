<?php

$filename = "C:\ProgramData\StoneNotes\storage.txt"; // file to store key-value pairs

// If there's a POST request with both key and value
if ($_SERVER["REQUEST_METHOD"] == "POST" && isset($_POST["key"]) && isset($_POST["value"])) {
    $key = $_POST["key"];
    $value = $_POST["value"];
    
    $data = json_decode(file_get_contents($filename), true) ?? [];
    $data[$key] = $value;
    file_put_contents($filename, json_encode($data));
    
    echo "{'result': 1}";
    exit;
}

// If there's a GET request with key
if ($_SERVER["REQUEST_METHOD"] == "GET" && isset($_GET["key"])) {
    $key = $_GET["key"];
    $data = json_decode(file_get_contents($filename), true) ?? [];
    
    if (isset($data[$key])) {
        echo $data[$key];
    }
    exit;
}

echo "{'result': 0, 'error': 'Invalid request'}";
