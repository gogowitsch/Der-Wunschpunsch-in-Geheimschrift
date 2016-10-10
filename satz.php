<?php
$sText = join(' ', array_map(function($input) {
    return preg_replace("/([a-z])- +/", "$1", $input);
}, file('Michael Ende - Der Wunschpunsch.txt')));

$sText = str_replace(array("\n", "\r"), "", $sText);

$aSaetze = explode(".", $sText);
$iSatz = rand(0, count($aSaetze) - 1);

$sSatz = '';
while(strlen($sSatz) < 150) $sSatz .= $aSaetze[$iSatz++] . ".";

header('Access-Control-Allow-Origin: *');
echo json_encode(array('anzahlSaetze' => count($aSaetze), 'aktuellerSatzIndex'=> $iSatz, 'echterSatz' => trim(str_replace("  ", " ", $sSatz))));
